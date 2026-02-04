package actors.merging

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import data.LocalClusteringMerge
import protocol.{GlobalMergeProtocol, OrchestratorProtocol, StatsProtocol}
import utils.CpuTime

import java.time.Instant
import scala.collection.mutable.ArrayBuffer

object GlobalMerger {

  final val DEFAULT_NAME: String = "GlobalMerger"

  def apply(
      orchestrator: ActorRef[OrchestratorProtocol.OrchestratorRequest],
      numEpsilonMerges: Int,
      stats: ActorRef[StatsProtocol.StatsRequest]
  ): Behavior[GlobalMergeProtocol.GlobalMergeRequest] = {
    val initialState = State(ArrayBuffer.empty[LocalClusteringMerge], numEpsilonMerges)
    Behaviors.setup { context =>
      new GlobalMerger(context, orchestrator, stats, initialState).behavior
    }
  }

  private case class State(localMergeResults: ArrayBuffer[LocalClusteringMerge], numEpsilonMerges: Int)

}

class GlobalMerger private (
    context: ActorContext[GlobalMergeProtocol.GlobalMergeRequest],
    orchestrator: ActorRef[OrchestratorProtocol.OrchestratorRequest],
    stats: ActorRef[StatsProtocol.StatsRequest],
    state: GlobalMerger.State
) {

  def behavior: Behavior[GlobalMergeProtocol.GlobalMergeRequest] =
    Behaviors.receiveMessage {
      case GlobalMergeProtocol.SendPairwiseMergeResult(localClusteringMerge) =>
        handleSendPairwiseMergeResult(context, localClusteringMerge)
      case _ => Behaviors.same
    }

  private def handleSendPairwiseMergeResult(
      context: ActorContext[GlobalMergeProtocol.GlobalMergeRequest],
      localClusteringMerge: LocalClusteringMerge
  ): Behavior[GlobalMergeProtocol.GlobalMergeRequest] = {
    state.localMergeResults += localClusteringMerge
    if (state.localMergeResults.size == state.numEpsilonMerges) {
      context.log.debug("All local merge results received, performing global merge")
      val startTime         = System.currentTimeMillis()
      val startCpu = CpuTime.nowNanos
      val globalMergeResult = merge(state.localMergeResults)
      val endCpu = CpuTime.nowNanos
      val endTime           = System.currentTimeMillis()
      val cpuTimeMs = CpuTime.toMillis(endCpu - startCpu)
      stats ! StatsProtocol.ReportGlobalMergingTime(
        Instant.ofEpochMilli(startTime),
        Instant.ofEpochMilli(endTime),
        state.numEpsilonMerges,
        cpuTimeMs
      )
      context.log.debug(s"Global merge result: $globalMergeResult")
      orchestrator ! OrchestratorProtocol.FinishedGlobalMerge(globalMergeResult)
    }
    Behaviors.same
  }

  private def merge(localClusteringMerges: ArrayBuffer[LocalClusteringMerge]): Map[(Int, Int), Int] =
    GlobalClusterMerger.globalMerge(localClusteringMerges.toArray)

}
