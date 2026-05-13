package components.union_find

import java.util.concurrent.atomic.{AtomicInteger, AtomicLongArray}

/** Lock-free union-find mirroring the C++ variant. Each entry: [lock bit 63][rank bits 32..62 (31 bits)][parent bits
  * 0..31] Indices supplied to methods must be in 0 until size. Adapted from:
  * https://github.com/wjakob/dset/blob/master/dset.h
  */
final class DisjointSets(size: Int) {

  private val data = {
    val arr = new AtomicLongArray(size)
    var i   = 0
    while (i < size) {
      arr.set(i, encode(rank = 0, parent = i, locked = false))
      i += 1
    }
    arr
  }

  private val components = new AtomicInteger(size)

  private val lockFlag: Long = 1L << 63

  private def encode(rank: Int, parent: Int, locked: Boolean): Long =
    ((rank.toLong & 0x7fffffffL) << 32) |
      (parent.toLong & 0xffffffffL) |
      (if (locked) lockFlag else 0L)

  private def withParent(value: Long, newParent: Int): Long =
    (value & 0xffffffff00000000L) | (newParent.toLong & 0xffffffffL)

  def size(): Int = size

  def parent(id: Int): Int =
    (data.get(id) & 0xffffffffL).toInt

  def rank(id: Int): Int =
    ((data.get(id) >>> 32) & 0x7fffffffL).toInt

  def isLocked(id: Int): Boolean =
    (data.get(id) & lockFlag) != 0L

  def find(id0: Int): Int = {
    var id = id0
    var p  = parent(id)
    while (id != p) {
      val value    = data.get(id)
      val gp       = parent(p)
      val newValue = withParent(value, gp)
      if (value != newValue) {
        data.compareAndSet(id, value, newValue) // path halving
      }
      id = gp
      p = parent(id)
    }
    id
  }

  def unite(a0: Int, b0: Int): Int = {
    var a = a0
    var b = b0
    while (true) {
      a = find(a)
      b = find(b)
      if (a == b) return a

      var r1 = rank(a)
      var r2 = rank(b)

      // Normalize so that 'a' attaches to 'b'
      if (r1 > r2 || (r1 == r2 && a < b)) {
        val tmpR = r1;
        r1 = r2;
        r2 = tmpR
        val tmpId = a; a = b; b = tmpId
      }

      val oldEntry = encode(r1, a, locked = false)
      val newEntry = encode(r1, b, locked = false)

      if (!data.compareAndSet(a, oldEntry, newEntry)) {
        // retry
      } else {
        components.decrementAndGet()
        if (r1 == r2) {
          val oldRoot = encode(r2, b, locked = false)
          val newRoot = encode(r2 + 1, b, locked = false)
          data.compareAndSet(b, oldRoot, newRoot)
        }
        return b
      }
    }
    -1
  }

  def try_lock(id0: Int): (Boolean, Int) = {
    var id    = find(id0)
    val value = data.get(id)
    if ((value & lockFlag) != 0L || parent(id) != id) return (false, id)
    val newValue = value | lockFlag
    if (data.compareAndSet(id, value, newValue)) (true, id)
    else (false, find(id))
  }

  def unlock(id: Int): Unit = {
    var cur = data.get(id)
    while ((cur & lockFlag) != 0L) {
      val newV = cur & ~lockFlag
      if (data.compareAndSet(id, cur, newV)) return
      cur = data.get(id)
    }
  }

  def unite_index_locked(id1: Int, id2: Int): Int = {
    val r1 = rank(id1); val r2 = rank(id2)
    if (r1 > r2 || (r1 == r2 && id1 < id2)) id1 else id2
  }

  def unite_unlock(id1: Int, id2: Int): Int = {
    var a  = id1;
    var b  = id2
    var r1 = rank(a); var r2 = rank(b)
    if (r1 > r2 || (r1 == r2 && a < b)) {
      val tmpR = r1;
      r1 = r2;
      r2 = tmpR
      val tmpId = a; a = b; b = tmpId
    }
    data.set(a, encode(r1, b, locked = false))
    val newRankB = r2 + (if (r1 == r2) 1 else 0)
    data.set(b, encode(newRankB, b, locked = false))
    components.decrementAndGet()
    b
  }

  def numSets(): Int = components.get()

}
