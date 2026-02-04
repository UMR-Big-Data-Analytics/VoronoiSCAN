package actors

import actors.clusterer.Clusterer
import actors.merging.EpsilonMerger
import akka.actor.typed._
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import data.{Edge, Vertex}
import dto.VoronoiCellCollectionDto
import protocol.{GuardianProtocol, OrchestratorProtocol, WorkerProtocol}

object Worker {

  final val DEFAULT_NAME = "worker"

  final val serviceKey: ServiceKey[WorkerProtocol.WorkerRequest] =
    ServiceKey[WorkerProtocol.WorkerRequest](DEFAULT_NAME)

  def apply(
             guardian: ActorRef[GuardianProtocol.GuardianRequest]
           ): Behavior[WorkerProtocol.WorkerRequest] = new Worker(guardian).behavior

}

private class Worker(guardian: ActorRef[GuardianProtocol.GuardianRequest]) {

  def behavior: Behavior[WorkerProtocol.WorkerRequest] =
    Behaviors.setup { context =>
      context.log.info("Worker actor started")
      context.system.receptionist ! Receptionist.register(Worker.serviceKey, context.self)
      Reaper.watchWithDefaultReaper(context.self)

      Behaviors
        .receiveMessage[WorkerProtocol.WorkerRequest] {
          case WorkerProtocol.SpawnWorker(
            replyTo, cellsDto, epsilonMergeEdges, epsilon, minPts, dbscanImpl, globalMerger, orchestrator,
            epsilonMergeManager, stats, writer
          ) =>
            val cells = VoronoiCellCollectionDto.toVoronoiCells(cellsDto)
            val clusterer = cells.map { cell =>
              val ref = context.spawn(
                Clusterer(cell, orchestrator, epsilonMergeManager, stats, writer, epsilon, minPts, dbscanImpl),
                s"${Clusterer.DEFAULT_NAME}-${cell.idx}",
                DispatcherSelector.fromConfig("akka.epsilon-extension-dispatcher")
              )
              context.watch(ref)
              ref
            }

            val epsilonMerger = epsilonMergeEdges.map { case (left, right) =>
              val ref =
                context.spawn(
                  EpsilonMerger(orchestrator, globalMerger, stats),
                  s"${EpsilonMerger.DEFAULT_NAME}-$left-$right"
                )
              Edge[Int, Vertex[Int]](new Vertex(left), new Vertex(right)) -> ref
            }.toMap
            replyTo ! OrchestratorProtocol.SpawnedWorker(clusterer, epsilonMerger)

            Behaviors.same
          case WorkerProtocol.ShutdownMessage() =>
            context.system.receptionist ! Receptionist.deregister(Worker.serviceKey, context.self)
            Behaviors.stopped
        }
        .receiveSignal {
          case (_, ChildFailed(_, _)) =>
            guardian ! GuardianProtocol.ShutdownMessage()
            Behaviors.same
          case (_, Terminated(ref)) =>
            context.log.debug("Actor {} terminated", ref.path)
            Behaviors.same
        }
    }

}
