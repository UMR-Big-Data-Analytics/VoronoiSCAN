package components.union_find

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.parallel.CollectionConverters._

case class Node[T](parent: T, rank: Int)

class LockFreeUnionFind[T](objects: collection.Set[T]) extends UnionFind[T] {

  // Each element maps to an AtomicReference of its Node (parent and rank).
  // This allows for atomic updates of both parent and rank.
  private val nodes: TrieMap[T, AtomicReference[Node[T]]] =
    TrieMap(objects.map(v => v -> new AtomicReference(Node(v, 0))).toSeq: _*)

  def find(v: T): T = {
    @tailrec
    def findRec(current: T): T = {
      val nodeRef = nodes(current)
      val node    = nodeRef.get()
      val parent  = node.parent

      if (parent == current) {
        current
      } else {
        // Path compression: try to set the parent directly to the grandparent.
        // This is a form of path splitting/halving which is safe in concurrent environments.
        val grandparent = nodes(parent).get().parent
        nodeRef.compareAndSet(node, node.copy(parent = grandparent))

        // Continue search from the original parent.
        findRec(parent)
      }
    }
    findRec(v)
  }

  // Unites the sets containing v1 and v2 using the union-by-rank heuristic. This implementation is lock-free.
  def union(v1: T, v2: T): Unit = {
    @tailrec
    def unionRec(): Boolean = {
      val root1 = find(v1)
      val root2 = find(v2)

      if (root1 == root2) {
        // Already in the same set.
        return false
      }

      val root1NodeRef = nodes(root1)
      val root1Node    = root1NodeRef.get()

      val root2NodeRef = nodes(root2)
      val root2Node    = root2NodeRef.get()

      // Ensure a consistent order of operations to prevent deadlock with other schemes.
      val (r1, n1Ref, n1, r2, n2Ref, n2) =
        if (root1.hashCode < root2.hashCode) (root1, root1NodeRef, root1Node, root2, root2NodeRef, root2Node)
        else (root2, root2NodeRef, root2Node, root1, root1NodeRef, root1Node)

      if (n1.rank < n2.rank) {
        // Attach the smaller rank tree to the root of the higher rank tree.
        if (n1Ref.compareAndSet(n1, n1.copy(parent = r2))) {
          true // Success
        } else {
          unionRec() // Retry on CAS failure
        }
      } else if (n1.rank > n2.rank) {
        // Attach the smaller rank tree to the root of the higher rank tree.
        if (n2Ref.compareAndSet(n2, n2.copy(parent = r1))) {
          true // Success
        } else {
          unionRec() // Retry on CAS failure
        }
      } else {
        // Ranks are equal. Attach r1 to r2 and increment r2's rank.
        if (n1Ref.compareAndSet(n1, n1.copy(parent = r2))) {
          n2Ref.compareAndSet(n2, n2.copy(rank = n2.rank + 1))
          true // Success
        } else {
          unionRec() // Retry on CAS failure
        }
      }
    }
    unionRec()
  }

  def numComponents: Int = objects.par.map(find).toSet.seq.size

  override def getComponents: collection.Set[collection.Set[T]] =
    objects.par
      .groupBy(find)
      .values
      .map(group => group.seq.toSet: collection.Set[T])
      .toSet[collection.Set[T]]
      .seq

}
