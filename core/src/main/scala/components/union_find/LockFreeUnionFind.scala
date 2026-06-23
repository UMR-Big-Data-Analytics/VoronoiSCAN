package components.union_find

import utils.Pool

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.parallel.CollectionConverters._

case class Node[T](parent: T, rank: Int)

class LockFreeUnionFind[T](objects: collection.Set[T]) extends UnionFind[T] {

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
        val grandparent = nodes(parent).get().parent
        nodeRef.compareAndSet(node, node.copy(parent = grandparent))

        findRec(parent)
      }
    }
    findRec(v)
  }

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

      val (r1, n1Ref, n1, r2, n2Ref, n2) =
        if (root1.hashCode < root2.hashCode) (root1, root1NodeRef, root1Node, root2, root2NodeRef, root2Node)
        else (root2, root2NodeRef, root2Node, root1, root1NodeRef, root1Node)

      if (n1.rank < n2.rank) {
        if (n1Ref.compareAndSet(n1, n1.copy(parent = r2))) {
          true
        } else {
          unionRec()
        }
      } else if (n1.rank > n2.rank) {
        if (n2Ref.compareAndSet(n2, n2.copy(parent = r1))) {
          true
        } else {
          unionRec()
        }
      } else {
        if (n1Ref.compareAndSet(n1, n1.copy(parent = r2))) {
          n2Ref.compareAndSet(n2, n2.copy(rank = n2.rank + 1))
          true
        } else {
          unionRec()
        }
      }
    }
    unionRec()
  }

  def numComponents: Int = {
    val parObjs = objects.par
    parObjs.tasksupport = Pool.taskSupport
    parObjs.map(find).toSet.seq.size
  }

  override def getComponents: collection.Set[collection.Set[T]] = {
    val parObjs = objects.par
    parObjs.tasksupport = Pool.taskSupport
    parObjs
      .groupBy(find)
      .values
      .map(group => group.seq.toSet: collection.Set[T])
      .toSet[collection.Set[T]]
      .seq
  }

}
