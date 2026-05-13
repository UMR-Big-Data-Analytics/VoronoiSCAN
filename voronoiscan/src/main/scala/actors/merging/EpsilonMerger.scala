package actors.merging

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import dto.LocalDBSCANResultDto
import dto.localdbscanresult.{LocalDBSCANResultDtoProto => PBLocalDBSCANResultDto}
import protocol._
import serialization.LocalDBSCANResultDtoConv
import utils.CpuTime

import java.time.Instant
import scala.collection.mutable

object EpsilonMerger {

  final val DEFAULT_NAME: String = "EpsilonMerger"

  def apply(
      stateActor: ActorRef[OrchestratorProtocol.OrchestratorRequest],
      globalMergeActor: ActorRef[GlobalMergeProtocol.GlobalMergeRequest],
      statsActor: ActorRef[StatsProtocol.StatsRequest]
  ): Behavior[EpsilonMergeProtocol.EpsilonMergeRequest] = {
    val initialState = State(Array.fill(2)(None: Option[LocalDBSCANResultDto]), 2)
    Behaviors.setup { context =>
      new EpsilonMerger(context, stateActor, globalMergeActor, statsActor, initialState).behavior
    }
  }

  private case class State(results: Array[Option[LocalDBSCANResultDto]], var openResults: Int)

  private case class ChunkAssembly(
      expectedChunks: Int,
      buffers: mutable.Map[Int, Array[Byte]],
      received: Int
  )

}

class EpsilonMerger private (
    context: ActorContext[EpsilonMergeProtocol.EpsilonMergeRequest],
    stateActor: ActorRef[OrchestratorProtocol.OrchestratorRequest],
    globalMergeActor: ActorRef[GlobalMergeProtocol.GlobalMergeRequest],
    statsActor: ActorRef[StatsProtocol.StatsRequest],
    state: EpsilonMerger.State
) {

  // Track assemblies per cellIdx
  private val assemblies = mutable.Map[Int, EpsilonMerger.ChunkAssembly]()

  def behavior: Behavior[EpsilonMergeProtocol.EpsilonMergeRequest] =
    Behaviors.receiveMessage {
      case EpsilonMergeProtocol.PushDBSCANResultChunk(chunk, chunkIdx, totalChunks, cellIdx) =>
        val _ = assemblies.getOrElseUpdate(
          cellIdx,
          EpsilonMerger.ChunkAssembly(totalChunks, scala.collection.mutable.Map.empty, 0)
        )

        val current = assemblies(cellIdx)
        require(!current.buffers.contains(chunkIdx), s"Chunk with index $chunkIdx already received for cell $cellIdx")
        current.buffers(chunkIdx) = chunk
        assemblies.update(cellIdx, current.copy(received = current.received + 1))
        if (current.expectedChunks > 0 && current.received + 1 == current.expectedChunks) {
          val fullBytes = (0 until current.expectedChunks).flatMap(current.buffers(_)).toArray
          val pbDto = PBLocalDBSCANResultDto.parseFrom(fullBytes)
          val clusteringResultDto = LocalDBSCANResultDtoConv.fromProto(pbDto)
          if (handleDBSCANResult(context, clusteringResultDto)) {
            Behaviors.stopped
          } else {
            assemblies.remove(cellIdx)
          }
        }
        Behaviors.same
      case EpsilonMergeProtocol.PushDBSCANResult(clusteringResultDto) =>
        if (handleDBSCANResult(context, clusteringResultDto)) {
          Behaviors.stopped
        } else {
          Behaviors.same
        }

      case _ => Behaviors.same
    }

  private def handleDBSCANResult(
      context: ActorContext[EpsilonMergeProtocol.EpsilonMergeRequest],
      clusteringResult: LocalDBSCANResultDto
                                ): Boolean = {
    state.openResults -= 1
    context.log.debug(s"Received a clustering result. Open results: ${state.openResults}")
    state.results(state.openResults) = Some(clusteringResult)

    if (state.openResults == 0) {
      state.results match {
        case Array(Some(result1), Some(result2)) =>
          val start = Instant.now()
          val startCpu = CpuTime.nowNanos
          context.log.debug(s"Merging results $result1 and $result2")
          val localClusteringMerge = PairwiseMerger.merge(result1, result2)
          context.log.debug(localClusteringMerge.toString)
          val sentNetworkEdges = if (globalMergeActor.path.address.hasGlobalScope) {
            localClusteringMerge.merges.size
          } else {
            0L
          }
          globalMergeActor ! GlobalMergeProtocol.SendPairwiseMergeResult(localClusteringMerge)
          stateActor ! OrchestratorProtocol.EpsilonMergeCompleted()
          val end = Instant.now()
          val endCpu = CpuTime.nowNanos
          val cpuTimeMs = CpuTime.toMillis(endCpu - startCpu)
          statsActor ! StatsProtocol.ReportEpsilonMergingTime(
            (result1.cellIdx, result2.cellIdx),
            start,
            end,
            cpuTimeMs,
            sentNetworkEdges
          )
          return true
        case _ => throw new IllegalStateException("LocalClusteringResults not present. This should not happen")
      }
    }
    false
  }

}
