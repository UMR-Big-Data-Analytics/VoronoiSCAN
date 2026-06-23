package utils

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.ForkJoinTaskSupport

object Pool {

  private val parCollPool = new ForkJoinPool(
    Runtime.getRuntime.availableProcessors()
  )

  val taskSupport = new ForkJoinTaskSupport(parCollPool)

}
