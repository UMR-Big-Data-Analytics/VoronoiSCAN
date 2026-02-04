package actors.guardian

import java.io.RandomAccessFile
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Paths, StandardOpenOption}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

class CSVPointSink[T](filePath: String) extends ClusteredPointSink {

  private var buffers: List[MappedByteBuffer] = _

  private var fileChannel: FileChannel = _

  private var isInitialized: Boolean = false

  private val recordSize: Int = 11 // 10 digits + newline

  private val labelWidth: Int = 10

  private val maxBufferSize: Long = Integer.MAX_VALUE

  override def setNumberOfPoints(numPoints: Long): Unit = {
    super.setNumberOfPoints(numPoints)
    if (!isInitialized && numberOfPoints > 0) {
      initializeFile()
      isInitialized = true
    }
  }

  override def collect(ids: Array[Long], labels: Array[Int]): Unit = {
    if (!isInitialized) {
      throw new IllegalStateException("CSVPointSink not initialized. Call setNumberOfPoints() first.")
    }

    writeChunkDirectly(ids, labels)
  }

  override def getLabels: Option[Array[Int]] =
    // Since we're writing directly to file, we don't maintain labels in memory
    // Instead we return an empty array or could implement reading from file if needed
    None

  private def initializeFile(): Unit = {
    val path = Paths.get(filePath)
    val totalSize: Long = numberOfPoints * recordSize

    println(s"CSVPointSink: initializing file with $numberOfPoints records ($totalSize bytes) at $filePath")

    new RandomAccessFile(path.toFile, "rw").setLength(totalSize)

    fileChannel = FileChannel.open(path, StandardOpenOption.READ, StandardOpenOption.WRITE)

    val bufferList = new ListBuffer[MappedByteBuffer]()
    var position: Long = 0
    while (position < totalSize) {
      val sizeToMap = Math.min(maxBufferSize, totalSize - position)
      bufferList += fileChannel.map(FileChannel.MapMode.READ_WRITE, position, sizeToMap)
      position += sizeToMap
    }
    buffers = bufferList.toList

  }

  private def writeChunkDirectly(ids: Array[Long], labels: Array[Int]): Unit = {
    val startTime = System.nanoTime()
    val numWorkers = Math.min(Runtime.getRuntime.availableProcessors(), ids.length)
    val chunkSize = Math.max(1, ids.length / numWorkers)

    implicit val ec: ExecutionContext = ExecutionContext.global

    val futures = (0 until numWorkers).map { i =>
      val start = i * chunkSize
      val end = if (i == numWorkers - 1) ids.length else start + chunkSize

      if (start < ids.length) {
        Future {
          writeRecordsRange(ids, labels, start, end)
        }
      } else {
        Future.successful(())
      }
    }

    Future.sequence(futures).foreach { _ =>
      val endTime = System.nanoTime()
      val durationMs = (endTime - startTime) / 1_000_000
    }
  }

  private def writeRecordsRange(ids: Array[Long], labels: Array[Int], startIdx: Int, endIdx: Int): Unit = {
    val ASCII_DIGITS = (('0' to '9').map(_.toByte) ++ Seq(' '.toByte)).toArray
    val ASCII_SPACE = ASCII_DIGITS(10)
    val ASCII_NEWLINE = '\n'.toByte
    val ASCII_MINUS = '-'.toByte

    for (i <- startIdx until endIdx) {
      val id = ids(i)
      val clusterLabel = labels(i)
      val globalPosition = id * recordSize

      val isNegative = clusterLabel < 0
      var currentLabel = math.abs(clusterLabel)
      var j = labelWidth - 1

      // Helper function to write a byte at a specific position with buffer bounds checking
      def writeByte(position: Long, value: Byte): Unit = {
        val bufIdx = (position / maxBufferSize).toInt
        val offset = (position % maxBufferSize).toInt
        buffers(bufIdx).put(offset, value)
      }

      if (currentLabel == 0) {
        writeByte(globalPosition + j, ASCII_DIGITS(0))
        j -= 1
      } else {
        while (currentLabel > 0 && j >= 0) {
          writeByte(globalPosition + j, ASCII_DIGITS(currentLabel % 10))
          currentLabel /= 10
          j -= 1
        }
      }

      if (isNegative && j >= 0) {
        writeByte(globalPosition + j, ASCII_MINUS)
        j -= 1
      }

      while (j >= 0) {
        writeByte(globalPosition + j, ASCII_SPACE)
        j -= 1
      }

      writeByte(globalPosition + labelWidth, ASCII_NEWLINE)
    }
  }

  def close(): Unit = {
    if (isInitialized) {
      buffers.foreach(_.force())
      if (fileChannel != null) {
        fileChannel.close()
      }
    }
  }

}
