package utils

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

/** Extension methods for Array[T] where T is a Numeric type Provides basic linear algebra operations */
object ArrayOps {

  implicit class NumericArrayOps[T](val arr: Array[T])(implicit num: Fractional[T], ct: ClassTag[T]) {

    /** Add two arrays element-wise */
    def +(other: Array[T]): Array[T] = {
      require(arr.length == other.length, "Arrays must have the same length for addition")
      val result = new Array[T](arr.length)
      var i      = 0
      val len    = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        result(i) = num.plus(arr(i), other(i))
        result(i + 1) = num.plus(arr(i + 1), other(i + 1))
        result(i + 2) = num.plus(arr(i + 2), other(i + 2))
        result(i + 3) = num.plus(arr(i + 3), other(i + 3))
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        result(i) = num.plus(arr(i), other(i))
        i += 1
      }

      result
    }

    /** Subtract another array element-wise */
    def -(other: Array[T]): Array[T] = {
      require(arr.length == other.length, "Arrays must have the same length for subtraction")
      val result = new Array[T](arr.length)
      var i      = 0
      val len    = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        result(i) = num.minus(arr(i), other(i))
        result(i + 1) = num.minus(arr(i + 1), other(i + 1))
        result(i + 2) = num.minus(arr(i + 2), other(i + 2))
        result(i + 3) = num.minus(arr(i + 3), other(i + 3))
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        result(i) = num.minus(arr(i), other(i))
        i += 1
      }

      result
    }

    /** Multiply array by a scalar */
    def *(scalar: T): Array[T] = {
      val result = new Array[T](arr.length)
      var i      = 0
      val len    = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        result(i) = num.times(arr(i), scalar)
        result(i + 1) = num.times(arr(i + 1), scalar)
        result(i + 2) = num.times(arr(i + 2), scalar)
        result(i + 3) = num.times(arr(i + 3), scalar)
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        result(i) = num.times(arr(i), scalar)
        i += 1
      }

      result
    }

    /** Divide array by a scalar */
    def /(scalar: T): Array[T] = {
      require(num.toDouble(scalar) != 0, "Division by zero")
      val result = new Array[T](arr.length)
      var i      = 0
      val len    = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        result(i) = num.div(arr(i), scalar)
        result(i + 1) = num.div(arr(i + 1), scalar)
        result(i + 2) = num.div(arr(i + 2), scalar)
        result(i + 3) = num.div(arr(i + 3), scalar)
        i += 4
      }

      // Handle remaining elements
      while (i < len) {
        result(i) = num.div(arr(i), scalar)
        i += 1
      }

      result
    }

    /** Element-wise multiplication (Hadamard product) */
    def **(other: Array[T]): Array[T] = {
      require(arr.length == other.length, "Arrays must have the same length for element-wise multiplication")
      val result = new Array[T](arr.length)
      var i      = 0
      val len    = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        result(i) = num.times(arr(i), other(i))
        result(i + 1) = num.times(arr(i + 1), other(i + 1))
        result(i + 2) = num.times(arr(i + 2), other(i + 2))
        result(i + 3) = num.times(arr(i + 3), other(i + 3))
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        result(i) = num.times(arr(i), other(i))
        i += 1
      }

      result
    }

    /** Dot product with another array */
    def dot(other: Array[T]): T = {
      require(arr.length == other.length, "Arrays must have the same length for dot product")
      var sum = num.zero
      var i   = 0
      val len = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        sum = num.plus(sum, num.times(arr(i), other(i)))
        sum = num.plus(sum, num.times(arr(i + 1), other(i + 1)))
        sum = num.plus(sum, num.times(arr(i + 2), other(i + 2)))
        sum = num.plus(sum, num.times(arr(i + 3), other(i + 3)))
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        sum = num.plus(sum, num.times(arr(i), other(i)))
        i += 1
      }

      sum
    }

    /** Calculate the Euclidean distance to another array */
    def distance(other: Array[T]): Double = math.sqrt(distanceSquared(other))

    /** Calculate the squared distance to another array */
    def distanceSquared(other: Array[T]): Double = {
      require(arr.length == other.length, "Arrays must have the same length for distance calculation")
      var sum = 0.0
      var i = 0
      val len = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit = len - (len % unroll)

      while (i < limit) {
        val d0 = num.toFloat(arr(i)) - num.toFloat(other(i))
        val d1 = num.toFloat(arr(i + 1)) - num.toFloat(other(i + 1))
        val d2 = num.toFloat(arr(i + 2)) - num.toFloat(other(i + 2))
        val d3 = num.toFloat(arr(i + 3)) - num.toFloat(other(i + 3))
        sum += d0 * d0 + d1 * d1 + d2 * d2 + d3 * d3
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        val d = num.toFloat(arr(i)) - num.toFloat(other(i))
        sum += d * d
        i += 1
      }

      sum
    }

    /** Return a normalized version of the array (unit vector) */
    def normalize: Array[T] = {
      val n = norm
      require(n > 0, "Cannot normalize a zero-length vector")

      val result = new Array[T](arr.length)
      var i = 0
      val len = arr.length

      while (i < len) {
        result(i) = num.div(arr(i), num.fromInt(n.toInt)) // Fallback, may lose precision
        i += 1
      }

      result
    }

    /** Calculate the L2 norm (Euclidean length) */
    def norm: Double = math.sqrt(normSquared)

    /** Calculate the squared L2 norm (sum of squares) For efficiency, doesn't compute the square root */
    def normSquared: Double = {
      var sum = 0.0
      var i   = 0
      val len = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        val v0 = num.toFloat(arr(i))
        val v1 = num.toFloat(arr(i + 1))
        val v2 = num.toFloat(arr(i + 2))
        val v3 = num.toFloat(arr(i + 3))
        sum += v0 * v0 + v1 * v1 + v2 * v2 + v3 * v3
        i   += unroll
      }

      // Handle remaining elements
      while (i < len) {
        val v = num.toFloat(arr(i))
        sum += v * v
        i   += 1
      }

      sum
    }

    /** Calculate the mean of all elements */
    def mean: Double = {
      if (arr.isEmpty) 0.0
      else {
        var sum = 0.0
        var i   = 0
        val len = arr.length

        while (i < len) {
          sum += num.toFloat(arr(i))
          i   += 1
        }

        sum / len
      }
    }

    /** Calculate the sum of all elements */
    def sum: T = {
      var result = num.zero
      var i      = 0
      val len    = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        result = num.plus(result, arr(i))
        result = num.plus(result, arr(i + 1))
        result = num.plus(result, arr(i + 2))
        result = num.plus(result, arr(i + 3))
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        result = num.plus(result, arr(i))
        i += 1
      }

      result
    }

    /** In-place add another array element-wise */
    def +=(other: Array[T]): Array[T] = {
      require(arr.length == other.length, "Arrays must have the same length for addition")
      var i   = 0
      val len = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        arr(i) = num.plus(arr(i), other(i))
        arr(i + 1) = num.plus(arr(i + 1), other(i + 1))
        arr(i + 2) = num.plus(arr(i + 2), other(i + 2))
        arr(i + 3) = num.plus(arr(i + 3), other(i + 3))
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        arr(i) = num.plus(arr(i), other(i))
        i += 1
      }

      arr
    }

    /** In-place subtract another array element-wise */
    def -=(other: Array[T]): Array[T] = {
      require(arr.length == other.length, "Arrays must have the same length for subtraction")
      var i   = 0
      val len = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        arr(i) = num.minus(arr(i), other(i))
        arr(i + 1) = num.minus(arr(i + 1), other(i + 1))
        arr(i + 2) = num.minus(arr(i + 2), other(i + 2))
        arr(i + 3) = num.minus(arr(i + 3), other(i + 3))
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        arr(i) = num.minus(arr(i), other(i))
        i += 1
      }

      arr
    }

    /** In-place multiply by a scalar */
    def *=(scalar: T): Array[T] = {
      var i   = 0
      val len = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        arr(i) = num.times(arr(i), scalar)
        arr(i + 1) = num.times(arr(i + 1), scalar)
        arr(i + 2) = num.times(arr(i + 2), scalar)
        arr(i + 3) = num.times(arr(i + 3), scalar)
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        arr(i) = num.times(arr(i), scalar)
        i += 1
      }

      arr
    }

    /** Check if two arrays are equal (same length and element-wise equality) */
    def isTheSame(other: Array[T]): Boolean = {
      if (arr.length != other.length) return false

      var i   = 0
      val len = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit  = len - (len % unroll)

      while (i < limit) {
        if (
          num.compare(arr(i), other(i)) != 0 ||
          num.compare(arr(i + 1), other(i + 1)) != 0 ||
          num.compare(arr(i + 2), other(i + 2)) != 0 ||
          num.compare(arr(i + 3), other(i + 3)) != 0
        ) {
          return false
        }
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        if (num.compare(arr(i), other(i)) != 0) {
          return false
        }
        i += 1
      }

      true
    }

    def hash(): Int =
      MurmurHash3.arrayHash(arr)

  }

  // Additional convenience methods for Double arrays which are common in ML applications
  implicit class DoubleArrayOps(val arr: Array[Double]) {

    /** Scale all elements to the range [0, 1] */
    def minMaxScale: Array[Double] = {
      if (arr.isEmpty) return arr.clone()

      var min = arr(0)
      var max = arr(0)
      var i = 1
      val len = arr.length

      // Find min and max values
      while (i < len) {
        val value = arr(i)
        if (value < min) min = value
        if (value > max) max = value
        i += 1
      }

      // If all values are the same, return an array of 0.5
      if (max == min) {
        val result = new Array[Double](len)
        i = 0
        while (i < len) {
          result(i) = 0.5
          i += 1
        }
        return result
      }

      // Scale to [0, 1]
      val result = new Array[Double](len)
      val range = max - min
      i = 0
      while (i < len) {
        result(i) = (arr(i) - min) / range
        i += 1
      }

      result
    }

    /** Z-score normalization (standard scaling) */
    def standardize: Array[Double] = {
      if (arr.isEmpty) return arr.clone()
      val len = arr.length

      // Calculate mean
      var sum = 0.0
      var i = 0
      while (i < len) {
        sum += arr(i)
        i += 1
      }
      val mean = sum / len

      // Calculate standard deviation
      var variance = 0.0
      i = 0
      while (i < len) {
        val diff = arr(i) - mean
        variance += diff * diff
        i += 1
      }
      val stdDev = math.sqrt(variance / len)

      // If standard deviation is zero, return an array of zeros
      if (stdDev == 0.0) {
        return new Array[Double](len)
      }

      // Calculate z-scores
      val result = new Array[Double](len)
      i = 0
      while (i < len) {
        result(i) = (arr(i) - mean) / stdDev
        i += 1
      }

      result
    }

    /** In-place divide by a scalar */
    def /=(scalar: Double): Array[Double] = {
      require(scalar != 0, "Division by zero")
      var i = 0
      val len = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit = len - (len % unroll)

      while (i < limit) {
        arr(i) /= scalar
        arr(i + 1) /= scalar
        arr(i + 2) /= scalar
        arr(i + 3) /= scalar
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        arr(i) /= scalar
        i += 1
      }

      arr
    }

    /** In-place add another double array */
    def +=(other: Array[Double]): Array[Double] = {
      require(arr.length == other.length, "Arrays must have the same length for addition")
      var i = 0
      val len = arr.length

      // Unroll loop for better performance
      val unroll = 4
      val limit = len - (len % unroll)

      while (i < limit) {
        arr(i) += other(i)
        arr(i + 1) += other(i + 1)
        arr(i + 2) += other(i + 2)
        arr(i + 3) += other(i + 3)
        i += unroll
      }

      // Handle remaining elements
      while (i < len) {
        arr(i) += other(i)
        i += 1
      }

      arr
    }

  }

}
