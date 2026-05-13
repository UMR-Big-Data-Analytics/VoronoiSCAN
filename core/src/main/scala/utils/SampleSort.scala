package utils

import scala.reflect.ClassTag

object SampleSort {

  // Thresholds & constants
  private val QUICKSORT_THRESHOLD = 1000

  private val BLOCK_QUOTIENT = 2

  private val BUCKET_QUOTIENT = 2

  private val OVER_SAMPLE = 10

  // Simple in-place quicksort
  def quickSort[A](arr: Array[A], start: Int, end: Int)(implicit ord: Ordering[A]): Unit =
    if (start < end) {
      val pivotIdx = partition(arr, start, end)
      quickSort(arr, start, pivotIdx - 1)
      quickSort(arr, pivotIdx + 1, end)
    }

  // Merge for pivot counts
  def mergeSeq[A](sA: Array[A], sB: Array[A], sC: Array[Int], lA: Int, lB: Int)(implicit ord: Ordering[A]): Unit = {
    if (lA == 0 || lB == 0) return
    java.util.Arrays.fill(sC, 0)
    var iA = 0
    var iB = 0
    while (iA < lA && iB < lB) {
      if (ord.compare(sA(iA), sB(iB)) < 0) {
        sC(iB) += 1
        iA     += 1
      } else {
        iB += 1
      }
    }
    if (iA < lA) sC(iB) += (lA - iA)
  }

  // Simple hash
  def hashVal(a: Int): Int = (982451653L * a.toLong + 12345L).toInt

  def sampleSort[A: ClassTag](arr: Array[A])(implicit ord: Ordering[A]) = {
    val n = arr.length
    if (n < QUICKSORT_THRESHOLD) quickSort(arr, 0, n - 1)
    else {
      val sqrtN      = math.ceil(math.sqrt(n)).toInt
      val numBlocks  = sqrtN / BLOCK_QUOTIENT + 1
      val blockSize  = ((n - 1) / numBlocks) + 1
      val numBuckets = sqrtN / BUCKET_QUOTIENT + 1
      val sampleSize = numBuckets * OVER_SAMPLE

      // Select random samples and sort them
      val sampleSet = Array.tabulate(sampleSize)(j => arr(hashVal(j) % n))
      quickSort(sampleSet, 0, sampleSize - 1)

      // Build pivots
      val pivots = Array.tabulate(numBuckets - 1)(k => sampleSet(OVER_SAMPLE * k))

      // Sort blocks, merge with pivots for bucket counts
      val counts = Array.ofDim[Int](numBlocks * numBuckets)
      for (i <- 0 until numBlocks) {
        val offset = i * blockSize
        val size   = if (i < numBlocks - 1) blockSize else n - offset
        quickSort(arr, offset, offset + size - 1)
        val tmpA = arr.slice(offset, offset + size)
        mergeSeq(tmpA, pivots, counts.slice(i * numBuckets, i * numBuckets + numBuckets), size, numBuckets - 1)
      }

      // Compute offsets (prefix sums, then move data to buckets)
      val prefixCounts = counts.scanLeft(0)(_ + _).tail
      val B            = new Array[A](n)
      for (i <- arr.indices) B(i) = arr(i)
      // Bucket-based placement (naive approach here)
      for (i <- 0 until numBlocks) {
        val offset      = i * blockSize
        val size        = if (i < numBlocks - 1) blockSize else n - offset
        val blockArr    = arr.slice(offset, offset + size)
        val localCounts = counts.slice(i * numBuckets, i * numBuckets + numBuckets)
        var idxBase     = 0
        for (b <- localCounts.indices) {
          val blockCount = localCounts(b)
          val outStart   = prefixCounts(i * numBuckets + b) - blockCount
          for (c <- 0 until blockCount) {
            B(outStart + c) = blockArr(idxBase)
            idxBase += 1
          }
        }
      }

      // Sort within each bucket
      var start = 0
      for (b <- 0 until numBuckets) {
        val bucketEnd =
          if (b < numBuckets - 1) prefixCounts.indexWhere(_ > prefixCounts(b * numBlocks), (b + 1) * numBlocks)
          else numBlocks * numBuckets
        if (
          b == 0 || b == numBuckets - 1 ||
          ord.compare(pivots(b - 1), pivots((b - 1).max(0))) < 0
        )
          quickSort(B, start, prefixCounts(bucketEnd - 1) - 1)
        start = prefixCounts(bucketEnd - 1)
      }
      Array.copy(B, 0, arr, 0, n)
    }
  }

  // Convenience alias
  def compSort[A: ClassTag](arr: Array[A])(implicit ord: Ordering[A]): Unit = sampleSort(arr)

  private def partition[A](arr: Array[A], start: Int, end: Int)(implicit ord: Ordering[A]): Int = {
    val pivot = arr(end)
    var i     = start
    for (j <- start until end) {
      if (ord.compare(arr(j), pivot) <= 0) {
        val tmp = arr(i)
        arr(i) = arr(j)
        arr(j) = tmp
        i += 1
      }
    }
    val tmp2 = arr(i)
    arr(i) = arr(end)
    arr(end) = tmp2
    i
  }

}
