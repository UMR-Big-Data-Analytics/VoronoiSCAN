package singletons

abstract class AbstractGenericSingleton[T](initialValue: Option[T] = None) {

  protected var instance: Option[T] = initialValue

  def this(initialValue: T) = this(Some(initialValue))

  def get: T = instance match {
    case Some(value) => value
    case None        => throw new NoSuchElementException("Singleton instance is not set")
  }

  def set(newValue: T): Unit =
    instance = Some(newValue)

  def reset(): Unit =
    instance = None

}
