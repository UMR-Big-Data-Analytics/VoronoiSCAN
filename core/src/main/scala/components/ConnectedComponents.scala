package components

import components.union_find.StaticUnionFind
import data.{Edge, Vertex}

class ConnectedComponents[T] {

  def findConnectedComponents(graph: collection.Set[Edge[T, Vertex[T]]]): collection.Set[collection.Set[Vertex[T]]] = {
    if (graph.isEmpty) return Set.empty
    val vertices  = graph.flatMap(edge => Set(edge.left, edge.right))
    val unionFind = new StaticUnionFind(vertices)
    graph.foreach { case Edge(u, v) => unionFind.union(u, v) }
    unionFind.getComponents
  }

}
