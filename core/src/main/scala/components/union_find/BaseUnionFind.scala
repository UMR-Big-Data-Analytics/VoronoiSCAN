// File: components/union_find/BaseUnionFind.scala
package components.union_find

import scala.collection.mutable

abstract class BaseUnionFind[T] extends UnionFind[T] {

  protected val parent: mutable.Map[T, T] = mutable.Map()

  protected val rank: mutable.Map[T, Int] = mutable.Map()

  // Uses union by rank optimization to keep trees balanced
  def union(v1: T, v2: T): Unit = {
    val root1 = find(v1)
    val root2 = find(v2)

    if (root1 != root2) {
      val rank1 = rank.getOrElse(root1, 0)
      val rank2 = rank.getOrElse(root2, 0)

      if (rank1 < rank2) {
        parent(root1) = root2
      } else if (rank1 > rank2) {
        parent(root2) = root1
      } else {
        parent(root2) = root1
        rank(root1) = rank1 + 1
      }
    }
  }

  def find(v: T): T =
    parent.get(v) match {
      case Some(p_of_v) =>
        if (p_of_v != v) {
          parent(v) = find(p_of_v)
        }
        parent(v)
      case None =>
        throw new NoSuchElementException(
          s"Element $v not found in UnionFind structure. Ensure it was part of initial objects."
        )
    }

  def getComponents: collection.Set[collection.Set[T]]

}
