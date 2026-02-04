package utils

import data.Point.Embedding
import jdk.incubator.vector.{DoubleVector, FloatVector, VectorOperators, VectorSpecies}

object Distances {

  /** Computes the squared L2 distance between two vectors. This is a vectorized implementation using Java's Vector API.
   * Code is adapted from
   * https://alexklibisz.com/2023/02/25/accelerating-vector-operations-jvm-jdk-incubator-vector-project-panama
   */

  private val floatSpecies: VectorSpecies[java.lang.Float] = FloatVector.SPECIES_PREFERRED

  private val doubleSpecies: VectorSpecies[java.lang.Double] = DoubleVector.SPECIES_PREFERRED

  def euclideanDistanceSquared(v1: Array[Float], v2: Array[Float]): Double = {
    var sumSqrDiff = 0f
    var i = 0
    val bound = floatSpecies.loopBound(v1.length)
    var fv1: FloatVector = null
    var fv2: FloatVector = null
    var fv3: FloatVector = null

    while (i < bound) {
      fv1 = FloatVector.fromArray(floatSpecies, v1, i)
      fv2 = FloatVector.fromArray(floatSpecies, v2, i)
      fv3 = fv1.sub(fv2)
      // For some unknown reason, fv3.mul(fv3) is significantly faster than fv3.pow(2).
      sumSqrDiff += fv3.mul(fv3).reduceLanes(VectorOperators.ADD)

      i += floatSpecies.length
    }

    while (i < v1.length) {
      val diff = v1(i) - v2(i)
      sumSqrDiff = Math.fma(diff, diff, sumSqrDiff)

      i += 1
    }
    sumSqrDiff
  }

  def euclideanDistanceSquared(v1: Array[Double], v2: Array[Double]): Double = {
    var sumSqrDiff = 0.0
    var i = 0
    val bound = doubleSpecies.loopBound(v1.length)
    var dv1: DoubleVector = null
    var dv2: DoubleVector = null
    var dv3: DoubleVector = null

    while (i < bound) {
      dv1 = DoubleVector.fromArray(doubleSpecies, v1, i)
      dv2 = DoubleVector.fromArray(doubleSpecies, v2, i)
      dv3 = dv1.sub(dv2)
      // For some unknown reason, dv3.mul(dv3) is significantly faster than dv3.pow(2).
      sumSqrDiff += dv3.mul(dv3).reduceLanes(VectorOperators.ADD)

      i += doubleSpecies.length
    }

    while (i < v1.length) {
      val diff = v1(i) - v2(i)
      sumSqrDiff = Math.fma(diff, diff, sumSqrDiff)

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
    val bound = doubleSpecies.loopBound(v.length)
    var dv: DoubleVector = null

    while (i < bound) {
      dv = DoubleVector.fromArray(doubleSpecies, v, i)
      // Square the vector elements directly and sum them
      sumSqr += dv.mul(dv).reduceLanes(VectorOperators.ADD)

      i += doubleSpecies.length
    }

    // Handle remaining elements
    while (i < v.length) {
      sumSqr = Math.fma(v(i), v(i), sumSqr)
      i += 1
    }

    sumSqr
  }

  def normSquared(v: Embedding): Float = {
    var sumSqr = 0f
    var i = 0
    val bound = floatSpecies.loopBound(v.length)
    var fv: FloatVector = null

    while (i < bound) {
      fv = FloatVector.fromArray(floatSpecies, v, i)
      // Square the vector elements and sum them
      sumSqr += fv.mul(fv).reduceLanes(VectorOperators.ADD)

      i += floatSpecies.length
    }

    // Handle remaining elements
    while (i < v.length) {
      sumSqr = Math.fma(v(i), v(i), sumSqr)
      i += 1
    }

    sumSqr
  }

  def norm(v: Array[Double]): Double = math.sqrt(normSquared(v))

  def norm(v: Array[Float]): Float = math.sqrt(normSquared(v)).toFloat

}
