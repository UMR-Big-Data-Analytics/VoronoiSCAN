package components.union_find

import data.Vertex

import scala.collection.mutable

// Implementation of Union-Find that allows dynamic addition of vertices
class StreamingUnionFind[T] extends BaseUnionFind[Vertex[T]] {

  // Override union to handle potentially new vertices
  override def union(v1: Vertex[T], v2: Vertex[T]): Unit = {
    // Initialize both vertices if they're new
    ensureVertexInitialized(v1)
    ensureVertexInitialized(v2)
    // Delegate to parent class implementation
    super.union(v1, v2)
  }

  // Returns all connected components in the graph
  override def getComponents: collection.Set[collection.Set[Vertex[T]]] = {
    // Map to store root vertices and their component members
    val components = mutable.Map[Vertex[T], mutable.Set[Vertex[T]]]()

    // Track processed vertices to avoid duplicates
    val processed = mutable.Set[Vertex[T]]()

    parent.keys.foreach { vertex =>
      if (!processed.contains(vertex)) {
        // Find all vertices in this component using DFS or BFS
        val componentVertices = findConnectedVertices(vertex)
        val root              = find(vertex)

        // Add all vertices from this component to processed set
        processed ++= componentVertices

        // Store the component
        components(root) = componentVertices
      }
    }

    components.values.map(_.toSet).toSet
  }

  private def findConnectedVertices(start: Vertex[T]): mutable.Set[Vertex[T]] = {
    val component = mutable.Set[Vertex[T]]()
    val queue     = mutable.Queue[Vertex[T]](start)

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (!component.contains(current)) {
        component.add(current)

        // Find all neighbors through parent relationships
        val root = find(current)
        parent.keys.foreach { vertex =>
          if (find(vertex) == root && !component.contains(vertex)) {
            queue.enqueue(vertex)
          }
        }
      }
    }

    component
  }

  // Override find to handle potentially new vertices
  override def find(v: Vertex[T]): Vertex[T] = {
    // Initialize vertex if it's new
    ensureVertexInitialized(v)
    // Delegate to parent class implementation
    super.find(v)
  }

  // Helper method to initialize a new vertex if it hasn't been seen before
  // Sets the vertex as its own parent with rank 0
  private def ensureVertexInitialized(v: Vertex[T]): Unit =
    if (!parent.contains(v)) {
      parent(v) = v // Make vertex its own parent
      rank(v) = 0   // Initialize rank to 0
    }

}
