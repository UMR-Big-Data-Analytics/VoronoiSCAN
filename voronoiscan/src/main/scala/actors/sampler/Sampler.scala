package actors.sampler

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import protocol.OrchestratorProtocol.OrchestratorRequest
import protocol.{OrchestratorProtocol, SamplerProtocol}

object Sampler {

  final val DEFAULT_NAME: String = "Sampler"

  def apply(
      orchestrator: ActorRef[OrchestratorRequest],
      cellSampler: CellSampler
  ): Behavior[SamplerProtocol.SamplerRequest] =
    Behaviors.setup { context =>
      new Sampler(context, orchestrator, cellSampler).behavior
    }

}

class Sampler private (
    context: ActorContext[SamplerProtocol.SamplerRequest],
    orchestrator: ActorRef[OrchestratorRequest],
    sampler: CellSampler
) {

  def behavior: Behavior[SamplerProtocol.SamplerRequest] =
    Behaviors.receiveMessage {
      case SamplerProtocol.ExtractSample(numSamples, minDistance, filePath) =>
        context.log.info(s"Start sampling with sampler: $sampler")
        val (centers, kdTree, graph) = sampler.getCenters(filePath, numSamples, minDistance)
        orchestrator ! OrchestratorProtocol.ExtractedSamples(centers, kdTree, graph)
        Behaviors.same
      case _ =>
        Behaviors.same
    }

}
