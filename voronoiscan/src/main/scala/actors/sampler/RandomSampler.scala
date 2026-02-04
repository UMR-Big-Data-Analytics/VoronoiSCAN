package actors.sampler

import data.{DelaunayGraph, Point}
import spatial.index.KDTree
import utils.Utils

import java.io.RandomAccessFile
import java.util.Random
import scala.util.{Failure, Success, Using}

class RandomSampler(seed: Option[Int] = None) extends CellSampler {

  private val random = seed match {
    case Some(s) => new Random(s)
    case None => new Random()
  }

  override def getCenters(
                           filePath: String,
                           numSamples: Int,
                           minDistance: Float
                         ): (Array[Point], KDTree, DelaunayGraph) = {
    val lines = getRandomSamples(filePath, 500 * numSamples)
    extractSamples(lines, numSamples, minDistance)
  }

  private def extractSamples(lines: Array[String], numSamples: Int, minDistance: Float) = {
    val vectors = lines.map { line =>
      val values = line.split(",").map(_.toFloat)
      values
    }
    val points = Utils.addPointIds(vectors)
    val (centers, tree) = Utils.getSeedPoints(points, numSamples, minDistance)
    val graph = delaunay.DelaunayGraphBuilder.buildDelaunayGraph(centers)
    (centers, tree, graph)
  }

  private def getRandomSamples(filepath: String, k: Int, encoding: String = "UTF-8"): Array[String] = {
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
