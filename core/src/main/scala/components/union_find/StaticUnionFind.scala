package components.union_find

class StaticUnionFind[T](objects: collection.Set[T]) extends BaseUnionFind[T] {

  parent ++= objects.map(v => v -> v)
  rank ++= objects.map(v => v -> 0)

  def numComponents: Int = objects.map(find).size

  override def getComponents: collection.Set[collection.Set[T]] =
    objects.groupBy(find).values.map(_.toSet).toSet

}
