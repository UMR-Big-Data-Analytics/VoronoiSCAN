package components

import components.union_find.StreamingUnionFind
import data.{Edge, Vertex}

class StreamingConnectedComponents[T] {

  private val unionFind = new StreamingUnionFind[T]()

  def processEdge(edge: Edge[T, Vertex[T]]): Unit =
    unionFind.union(edge.left, edge.right)

  def getCurrentComponents: collection.Set[collection.Set[Vertex[T]]] =
    unionFind.getComponents

  def isConnected(v1: Vertex[T], v2: Vertex[T]): Boolean =
    unionFind.find(v1) == unionFind.find(v2)

}
