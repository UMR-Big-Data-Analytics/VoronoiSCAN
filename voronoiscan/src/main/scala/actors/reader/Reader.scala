package actors.reader

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import data.Point
import protocol.{OrchestratorProtocol, PartitionerProtocol, ReaderProtocol}

import scala.collection.mutable
import scala.io.{BufferedSource, Source}

object Reader {

  final val DEFAULT_NAME: String = "Reader"

  def apply(batchSize: Int): Behavior[ReaderProtocol.ReaderRequest] =
    Behaviors.setup { _ =>
      new Reader(batchSize).behavior
    }

}

class Reader private (batchSize: Int) {

  private val behavior: Behavior[ReaderProtocol.ReaderRequest] = {
    Behaviors.setup { context =>
      var num = 0
      var doneReading = false
      val partitonerRefs = mutable.Set.empty[akka.actor.typed.ActorRef[PartitionerProtocol.PartitionerRequest]]

      Behaviors.receiveMessage {
        case ReaderProtocol.ReadFile(filePath, replyTo) =>
          source = Some(Source.fromFile(filePath))
          linesIterator = source.get.getLines()
          val result = readNextBatch()
          currentBatch = result._2
          doneReading = result._1
          replyTo ! OrchestratorProtocol.ReaderInitialized()
          Behaviors.same

        case ReaderProtocol.RequestBatch(partitioner) =>
          partitonerRefs += partitioner
          context.log.debug("Requesting next batch of points.")
          if (doneReading) {
            partitioner ! PartitionerProtocol.EndOfFile()
            partitonerRefs.remove(partitioner)
            if (partitonerRefs.isEmpty) {
              context.log.info(s"Reader finished reading file, total points read: $num")
              Behaviors.stopped
            } else {
              Behaviors.same
            }
          } else {
            partitioner ! PartitionerProtocol.PartitionPoints(currentBatch)
            num += currentBatch.length
            val result = readNextBatch()
            currentBatch = result._2
            doneReading = result._1
            Behaviors.same
          }
      }
    }
  }

  private var source: Option[BufferedSource] = None

  private var batchId = 0

  private var linesIterator: Iterator[String] = Iterator.empty

  private var currentBatch: Array[Point] = Array.empty

  private def readNextBatch(): (Boolean, Array[Point]) =
    source match {
      case Some(_) =>
        // Skip the CSV header row
        if (batchId == 0) {
          linesIterator.next()
        }
        val batchLines = linesIterator.take(batchSize).toArray
        if (batchLines.isEmpty) {
          closeSource()
          (true, Array.empty)
        } else {
          val points = batchLines.zipWithIndex.map { case (line, idx) =>
            parseLine(line, batchId, idx, batchSize)
          }
          batchId += 1
          (false, points)
        }

      case None => throw new IllegalStateException("Source not initialized")
    }

  private def parseLine(line: String, batchId: Int, index: Int, batchSize: Int): Point = {
    val coords = line.split(",").map(_.toFloat)
    Point(coords, index + batchId * batchSize)
  }

  private def closeSource(): Unit = {
    source.foreach(_.close())
    source = None
    linesIterator = Iterator.empty
  }

}
