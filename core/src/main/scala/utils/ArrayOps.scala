package utils

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

object ArrayOps {

  implicit class NumericArrayOps[T](val arr: Array[T])(implicit num: Fractional[T], ct: ClassTag[T]) {

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

    def norm: Double = math.sqrt(normSquared)

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

}
