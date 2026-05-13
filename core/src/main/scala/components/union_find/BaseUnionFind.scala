// File: components/union_find/BaseUnionFind.scala
package components.union_find

import scala.collection.mutable

// Abstract class implementing Union-Find data structure for tracking connected components
// T is the type parameter for values stored in vertices
abstract class BaseUnionFind[T] extends UnionFind[T] {

  // Maps each vertex to its parent in the tree structure
  // Initially, each vertex is its own parent
  protected val parent: mutable.Map[T, T] = mutable.Map()

  // Stores the height (rank) of the tree rooted at each vertex
  // Used for union by rank optimization
  protected val rank: mutable.Map[T, Int] = mutable.Map()

  // Merges two sets by connecting their root nodes
  // Uses union by rank optimization to keep trees balanced
  def union(v1: T, v2: T): Unit = {
    // find will throw if v1 or v2 are not in `parent` map,
    // which is desired if they weren't initialized.
    val root1 = find(v1)
    val root2 = find(v2)

    if (root1 != root2) {
      // rank(rootX) is safe if initialization guarantees keys exist.
      // Using getOrElse for robustness if keys could somehow be missing.
      val rank1 = rank.getOrElse(root1, 0)
      val rank2 = rank.getOrElse(root2, 0)

      if (rank1 < rank2) {
        parent(root1) = root2
      } else if (rank1 > rank2) {
        parent(root2) = root1
      } else {
        parent(root2) = root1
        rank(root1) = rank1 + 1 // Same as rank(root1) += 1 from your original
      }
    }
  }

  // Finds the root vertex for the given vertex
  // Implements path compression: flattens the tree structure during traversal
  def find(v: T): T =
    parent.get(v) match {
      case Some(p_of_v) =>
        if (p_of_v != v) { // If v is not its own root
          parent(v) = find(p_of_v) // Path compression
        }
        parent(v) // Return the (now potentially direct) root of v
      case None =>
        // This occurs if 'v' was never added/initialized in the DSU.
        throw new NoSuchElementException(
          s"Element $v not found in UnionFind structure. Ensure it was part of initial objects."
        )
    }

  // Abstract method to be implemented by subclasses
  // Returns all connected components as sets of vertices
  def getComponents: collection.Set[collection.Set[T]]

}
