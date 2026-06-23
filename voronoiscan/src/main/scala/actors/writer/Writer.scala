package actors.writer

import actors.Reaper
import actors.guardian.ClusteredPointSink
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{ActorRef, Behavior}
import configuration.InputConfiguration
import dto.ClusterMapDto
import protocol.{ClustererProtocol, GuardianProtocol, WriterProtocol}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

object Writer {

  final val DEFAULT_NAME = "writer"

  def apply(
      pointSink: ClusteredPointSink
  ): Behavior[WriterProtocol.WriterRequest] =
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        new Writer(context, timers, pointSink).behavior
      }
    }

}

class Writer private(
                      context: ActorContext[WriterProtocol.WriterRequest],
                      timers: TimerScheduler[WriterProtocol.WriterRequest],
                      pointSink: ClusteredPointSink
                    ) {

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

      case WriterProtocol.LabeledPointsChunk(sourceCellIdx, ids, labels, chunkIdx, totalChunks, replyTo) =>
        if (!timers.isTimerActive(WriterProtocol.LabelingStuckKey)) {
          context.log.info("First LabeledPointsChunk received — starting labeling stuck detector (30s interval)")
          timers.startTimerWithFixedDelay(WriterProtocol.LabelingStuckKey, WriterProtocol.LabelingStuckCheck(), 30.seconds)
        }
        replyTo ! ClustererProtocol.LabeledChunkAck()
        state.addLabelsChunk(sourceCellIdx, ids, labels, chunkIdx, totalChunks)
        if (state.allLabelsReceived) {
          context.log.info("All label partitions received, preparing to stop VoronoiSCAN")
          timers.cancel(WriterProtocol.LabelingStuckKey)
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

      case WriterProtocol.LabelingStuckCheck() =>
        state.logStuckState()
        Behaviors.same
    }
  }

}

private class WriterState(val context: ActorContext[WriterProtocol.WriterRequest], pointSink: ClusteredPointSink) {

  private final case class ChunkSequence(
                                          totalChunks: Int,
                                          chunks: Array[(Array[Long], Array[Int])],
                                          received: Array[Boolean],
                                          var receivedCount: Int,
                                          sequenceId: Long
                                        )

  private val chunkSequencesBySource = mutable.Map.empty[Int, mutable.ArrayBuffer[ChunkSequence]]

  private var nextSequenceId: Long = 0L

  private var pointsLabeled = 0

  private var numLabelPartitionsToReceive: Int = -1

  private val completedSources = mutable.Set.empty[Int]

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

  def addLabelsChunk(sourceCellIdx: Int, ids: Array[Long], labels: Array[Int], chunkIdx: Int, totalChunks: Int): Unit = {
    if (chunkIdx < 0 || totalChunks <= 0 || chunkIdx >= totalChunks) {
      context.log.warn(
        s"Ignoring invalid chunk metadata from source $sourceCellIdx: chunkIdx=$chunkIdx totalChunks=$totalChunks"
      )
      return
    }

    val sequences = chunkSequencesBySource.getOrElseUpdate(sourceCellIdx, mutable.ArrayBuffer.empty[ChunkSequence])

    val matchingSequence = sequences
      .find(seq => seq.totalChunks == totalChunks && !seq.received(chunkIdx))

    val active = matchingSequence.getOrElse {
      val seq = ChunkSequence(
        totalChunks = totalChunks,
        chunks = new Array[(Array[Long], Array[Int])](totalChunks),
        received = Array.fill(totalChunks)(false),
        receivedCount = 0,
        sequenceId = nextSequenceId
      )
      nextSequenceId += 1
      sequences += seq
      if (chunkIdx != 0) {
        context.log.warn(
          s"Late-start sequence created for source $sourceCellIdx at chunk $chunkIdx/$totalChunks (seq=${seq.sequenceId})"
        )
      }
      seq
    }

    if (!active.received(chunkIdx)) {
      active.chunks(chunkIdx) = (ids, labels)
      active.received(chunkIdx) = true
      active.receivedCount += 1
    } else {
      context.log.warn(
        s"Duplicate chunk $chunkIdx/$totalChunks from source $sourceCellIdx ignored (seq=${active.sequenceId})"
      )
    }

    if (active.receivedCount == active.totalChunks) {
      val allIds = active.chunks.iterator.flatMap(_._1).toArray
      val allLabels = active.chunks.iterator.flatMap(_._2).toArray
      context.log.info(
        s"Partition completed for source cell $sourceCellIdx (seq=${active.sequenceId}), ${allIds.length} points. " +
          s"Remaining partitions: ${numLabelPartitionsToReceive - 1}"
      )
      completedSources += sourceCellIdx
      collectLabels(allLabels, allIds)
      sequences -= active
      if (sequences.isEmpty)
        chunkSequencesBySource.remove(sourceCellIdx)
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

  def logStuckState(): Unit = {
    if (numLabelPartitionsToReceive > 0) {
      val inFlight = chunkSequencesBySource.keys.toSeq.sorted
      val inFlightDetails = inFlight.map { cell =>
        val seqs = chunkSequencesBySource(cell)
        val seqDetails = seqs.map { seq =>
          val missingChunks = seq.received.zipWithIndex.collect { case (false, idx) => idx }
          s"(seq=${seq.sequenceId}, total=${seq.totalChunks}, got=${seq.receivedCount}, missing=${missingChunks.mkString("[", ",", "]")})"
        }
        s"cell $cell: ${seqDetails.mkString(", ")}"
      }
      val completedCount = completedSources.size
      val totalExpected = completedCount + numLabelPartitionsToReceive
      context.log.warn(
        s"[STUCK] Labeling stuck: $numLabelPartitionsToReceive/$totalExpected partitions outstanding. " +
          s"Completed sources: ${completedSources.toSeq.sorted.mkString("[", ",", "]")}. " +
          s"In-flight: ${inFlightDetails.mkString("; ")}. " +
          s"No-activity sources (expected but never seen): missing from both completed and in-flight."
      )
    } else if (numLabelPartitionsToReceive == 0) {
      context.log.info("[STUCK-CHECK] All label partitions already received — no issue.")
    } else {
      context.log.warn(s"[STUCK-CHECK] numLabelPartitionsToReceive=$numLabelPartitionsToReceive (negative!) — overcounting bug.")
    }
  }

}
