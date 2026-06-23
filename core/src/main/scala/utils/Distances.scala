package utils

import data.Point.Embedding

object Distances {

  def euclideanDistanceSquared(v1: Array[Float], v2: Array[Float]): Double = {
    var sumSqrDiff = 0.0
    var i = 0
    val len = v1.length
    while (i < len) {
      val diff = (v1(i) - v2(i)).toDouble
      sumSqrDiff += diff * diff
      i += 1
    }
    sumSqrDiff
  }

  def euclideanDistanceSquared(v1: Array[Double], v2: Array[Double]): Double = {
    var sumSqrDiff = 0.0
    var i = 0
    val len = v1.length
    while (i < len) {
      val diff = v1(i) - v2(i)
      sumSqrDiff += diff * diff
      i += 1
    }
    sumSqrDiff
  }

  def euclideanDistance(a: Array[Double], b: Array[Double]): Double =
    math.sqrt(euclideanDistanceSquared(a, b))

  def euclideanDistance(a: Array[Float], b: Array[Float]): Float =
    math.sqrt(euclideanDistanceSquared(a, b)).toFloat

  def normSquared(v: Array[Double]): Double = {
    var sumSqr = 0.0
    var i = 0
    val len = v.length
    while (i < len) {
      sumSqr += v(i) * v(i)
      i += 1
    }
    sumSqr
  }

  def normSquared(v: Embedding): Float = {
    var sumSqr = 0.0f
    var i = 0
    val len = v.length
    while (i < len) {
      sumSqr += v(i) * v(i)
      i += 1
    }
    sumSqr
  }

  def norm(v: Array[Double]): Double = math.sqrt(normSquared(v))

  def norm(v: Array[Float]): Float = math.sqrt(normSquared(v)).toFloat

}
