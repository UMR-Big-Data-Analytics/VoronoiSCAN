package components.union_find

class StaticUnionFind[T](objects: collection.Set[T]) extends BaseUnionFind[T] {

  // Initialize each vertex as its own parent (reflexive relationship)
  parent ++= objects.map(v => v -> v)
  // Initialize each vertex with rank 0
  rank ++= objects.map(v => v -> 0)

  def numComponents: Int = objects.map(find).size

  override def getComponents: collection.Set[collection.Set[T]] =
    // Groups vertices by their root (found using find method)
    // and converts each group to a Set
    objects.groupBy(find).values.map(_.toSet).toSet

}
