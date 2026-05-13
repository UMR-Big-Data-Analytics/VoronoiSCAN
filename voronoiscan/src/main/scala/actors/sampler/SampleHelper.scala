package actors.sampler

import data.Point
import utils.Utils

import java.io.RandomAccessFile
import java.util.Random
import scala.util.{Failure, Success, Using}

object SampleHelper {

  def getPoints(lines: Array[String]): Array[Point] = {
    val vectors = lines.map { line =>
      val values = line.split(",").map(_.toFloat)
      values
    }
    Utils.addPointIds(vectors)
  }

  def getRandomSamples(
      filepath: String,
      k: Int,
      seed: Option[Int] = None,
      encoding: String = "UTF-8"
  ): Array[String] = {
    val random = seed match {
      case Some(s) => new Random(s)
      case None    => new Random()
    }
    Using(new RandomAccessFile(filepath, "r")) { raf =>
      val fileSize = raf.length()
      val samples  = new Array[String](k)

      if (fileSize == 0) {
        throw new IllegalStateException("Cannot sample from an empty file.")
      }

      for (i <- 0 until k) {
        var line: String = null
        // Loop until a non-empty line is found. This handles seeking to the
        // very end of the file where readLine() would return null.
        while (line == null || line.trim.isEmpty) {
          // Use the instance random instead of the global Random
          val randomOffset = math.abs(random.nextLong()) % fileSize
          raf.seek(randomOffset)

          //  Discard the first partial line, as we likely landed in the middle of it.
          raf.readLine()

          //  Read the next full line, which is our random sample.
          line = raf.readLine()
        }

        val correctlyEncodedLine = new String(line.getBytes("ISO-8859-1"), encoding)
        samples(i) = correctlyEncodedLine
      }
      samples
    } match {
      case Success(value) => value
      case Failure(exception) =>
        throw new RuntimeException(
          s"Error while sampling $k lines from file $filepath: ${exception.getMessage}",
          exception
        )
    }
  }

}
