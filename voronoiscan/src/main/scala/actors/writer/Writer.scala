package actors.writer

import actors.Reaper
import actors.guardian.ClusteredPointSink
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import configuration.InputConfiguration
import dto.ClusterMapDto
import protocol.{ClustererProtocol, GuardianProtocol, WriterProtocol}

import scala.collection.mutable

object Writer {

  final val DEFAULT_NAME = "writer"

  def apply(
      pointSink: ClusteredPointSink
  ): Behavior[WriterProtocol.WriterRequest] =
    Behaviors.setup { context =>
      new Writer(context, pointSink).behavior
    }

}

class Writer private (context: ActorContext[WriterProtocol.WriterRequest], pointSink: ClusteredPointSink) {

  def behavior: Behavior[WriterProtocol.WriterRequest] = {
    context.log.info("Writer actor started")
    Reaper.watchWithDefaultReaper(context.self)

    val state = new WriterState(context, pointSink)

    Behaviors.receiveMessage[WriterProtocol.WriterRequest] {
      case WriterProtocol.InitializeWriter(askRef, guardian, parameters) =>
        state.initialize(askRef, guardian, parameters)
        Behaviors.same

      case WriterProtocol.ReportNumberOfCells(numCells) =>
        context.log.info(s"Number of cells reported: $numCells")
        state.setNumLabelPartitionsToReceive(numCells)
        Behaviors.same

      case WriterProtocol.ReportNumberOfPoints(numPoints) =>
        state.reportNumberOfPoints(numPoints)
        Behaviors.same

      case WriterProtocol.LabeledPointsChunk(ids, labels, chunkIdx, totalChunks) =>
        state.addLabelsChunk(ids, labels, chunkIdx, totalChunks)
        if (state.allLabelsReceived) {
          context.log.info("All label partitions received, preparing to stop VoronoiSCAN")
          state.completeLabeling()
        }
        Behaviors.same

      case WriterProtocol.StartLabeling(clusterMapDto, clusterers) =>
        context.log.info("Starting labeling phase with {} clusterers", clusterers.length)
        state.startLabeling(clusterMapDto, clusterers)
        Behaviors.same

      case WriterProtocol.ShutdownWriter() =>
        pointSink.close()
        Behaviors.stopped
    }
  }

}

private class WriterState(val context: ActorContext[WriterProtocol.WriterRequest], pointSink: ClusteredPointSink) {

  // Buffer for chunked label partitions - using a unique key per chunk sequence
  private val chunkBuffers = mutable.Map.empty[String, mutable.ArrayBuffer[(Array[Long], Array[Int])]]

  private val chunkCounts = mutable.Map.empty[String, (Int, Int)] // sequenceKey -> (receivedChunks, totalChunks)

  private var pointsLabeled = 0

  private var numLabelPartitionsToReceive: Int = -1

  private var sequenceCounter = 0

  private var askRef: Option[ActorRef[GuardianProtocol.StopVoronoiSCAN]] = None

  private var guardian: Option[ActorRef[GuardianProtocol.GuardianRequest]] = None

  private var parameters: Option[InputConfiguration] = None

  def initialize(
      askRefParam: Option[ActorRef[GuardianProtocol.StopVoronoiSCAN]],
      guardianParam: ActorRef[GuardianProtocol.GuardianRequest],
      parametersParam: InputConfiguration
  ): Unit = {
    askRef = askRefParam
    guardian = Some(guardianParam)
    parameters = Some(parametersParam)
  }

  def setNumLabelPartitionsToReceive(numPartitions: Int): Unit =
    numLabelPartitionsToReceive = numPartitions

  def startLabeling(
      clusterMapDto: ClusterMapDto,
      clusterers: Array[ActorRef[ClustererProtocol.ClustererRequest]]
  ): Unit =
    clusterers.foreach(_ ! ClustererProtocol.LabelPoints(clusterMapDto))

  def addLabelsChunk(ids: Array[Long], labels: Array[Int], chunkIdx: Int, totalChunks: Int): Unit = {
    // Create a unique sequence key for this chunk stream
    val sequenceKey = if (chunkIdx == 0) {
      // First chunk - create new sequence
      val newKey = s"seq_${sequenceCounter}_total_$totalChunks"
      sequenceCounter += 1
      chunkCounts(newKey) = (0, totalChunks)
      chunkBuffers(newKey) = mutable.ArrayBuffer.empty
      newKey
    } else {
      // Find the sequence that expects this chunk index and has matching totalChunks
      chunkCounts.collectFirst {
        case (key, (received, total)) if total == totalChunks && received == chunkIdx && received < total => key
      }.getOrElse {
        // Emergency fallback - this shouldn't happen in normal operation
        context.log.error(s"Emergency: Creating new sequence for chunk $chunkIdx/$totalChunks")
        val emergencyKey = s"emergency_${sequenceCounter}_total_$totalChunks"
        sequenceCounter += 1
        chunkCounts(emergencyKey) = (chunkIdx, totalChunks)
        chunkBuffers(emergencyKey) = mutable.ArrayBuffer.empty
        emergencyKey
      }
    }

    val buffer = chunkBuffers(sequenceKey)
    buffer += ((ids, labels))
    val (received, total) = chunkCounts(sequenceKey)
    chunkCounts(sequenceKey) = (received + 1, total)

    context.log.debug(s"Received chunk $chunkIdx/$totalChunks for sequence $sequenceKey, now have ${chunkCounts(sequenceKey)._1}/${chunkCounts(sequenceKey)._2} chunks")

    // If all chunks for this sequence received, merge and collect
    if (chunkCounts(sequenceKey)._1 == totalChunks) {
      val allIds    = buffer.flatMap(_._1).toArray
      val allLabels = buffer.flatMap(_._2).toArray
      context.log.debug(
        s"Sequence $sequenceKey complete, processing ${allIds.length} points. Remaining partitions: ${numLabelPartitionsToReceive - 1}"
      )
      collectLabels(allLabels, allIds)
      chunkBuffers.remove(sequenceKey)
      chunkCounts.remove(sequenceKey)
    }
  }

  private def collectLabels(labels: Array[Int], ids: Array[Long]): Unit = {
    numLabelPartitionsToReceive -= 1
    pointsLabeled               += labels.length
    pointSink.collect(ids, labels)
  }

  def allLabelsReceived: Boolean = numLabelPartitionsToReceive == 0

  def completeLabeling(): Unit = {
    askRef match {
      case Some(ref) =>
        val labels = pointSink.getLabels
        ref ! GuardianProtocol.StopVoronoiSCAN(labels)
      case None =>
    }
    guardian.foreach(_ ! GuardianProtocol.ShutdownMessage())
  }

  def reportNumberOfPoints(numPoints: Long): Any =
    pointSink.setNumberOfPoints(numPoints)

}
