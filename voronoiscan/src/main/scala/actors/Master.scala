package actors

import actors.guardian.ClusteredPointSink
import actors.orchestrator.Orchestrator
import actors.writer.Writer
import akka.actor.typed._
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.Behaviors
import configuration.{InputConfiguration, SystemConfiguration}
import protocol._

object Master {

  final val DEFAULT_NAME = "master"

  def apply(
      guardian: ActorRef[GuardianProtocol.GuardianRequest],
      pointSink: ClusteredPointSink
  ): Behavior[MasterProtocol.MasterRequest] =
    new Master(guardian, pointSink).behavior

}

class Master private (guardian: ActorRef[GuardianProtocol.GuardianRequest], pointSink: ClusteredPointSink) {

  def behavior: Behavior[MasterProtocol.MasterRequest] =
    Behaviors.setup { context =>
      val listingResponseAdapter = context.messageAdapter(MasterProtocol.ReceptionistListingMessage(_))
      context.system.receptionist ! Receptionist.subscribe(Worker.serviceKey, listingResponseAdapter)
      context.log.info("Master actor started")

      val writer = context.spawn(
        Writer(pointSink),
        Writer.DEFAULT_NAME
      )
      context.watch(writer)

      val orchestrator: ActorRef[OrchestratorProtocol.OrchestratorRequest] = {
        context.spawn(
          Orchestrator(context.self, writer),
          Orchestrator.DEFAULT_NAME,
          DispatcherSelector.fromConfig("akka.master-pinned-dispatcher")
        )
      }
      context.watch(orchestrator)

      val state = new State(writer)

      Behaviors
        .receiveMessage[MasterProtocol.MasterRequest] {
          case MasterProtocol.StartMessage(ref, parameters) =>
            state.setAskRef(ref)
            state.setParameters(parameters)
            state.markForExecution()
            writer ! WriterProtocol.InitializeWriter(ref, guardian, parameters)
            startExecutionIfReady(orchestrator, state)
            Behaviors.same
          case MasterProtocol.ReceptionistListingMessage(listing) =>
            val workers = listing.serviceInstances(Worker.serviceKey)
            state.setWorkerRefs(workers)
            startExecutionIfReady(orchestrator, state)
            Behaviors.same
          case MasterProtocol.ShutdownMessage() =>
            orchestrator ! OrchestratorProtocol.Shutdown()
            writer ! WriterProtocol.ShutdownWriter()
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

  private def startExecutionIfReady(
      orchestrator: ActorRef[OrchestratorProtocol.OrchestratorRequest],
      state: State
  ): Unit =
    if (state.executionConditionsMet) {
      orchestrator ! OrchestratorProtocol.ExecuteVoronoiSCAN(state.getParameters, state.getWorkers)
      state.markExecuting()
    }

}

private class State(val writer: ActorRef[WriterProtocol.WriterRequest]) {

  private var workerRefs = Set.empty[ActorRef[WorkerProtocol.WorkerRequest]]

  private var askRef: Option[ActorRef[GuardianProtocol.StopVoronoiSCAN]] = None

  private var shouldStartExecution = false

  private var isExecuting = false

  private var parameters: Option[InputConfiguration] = None

  def getParameters: InputConfiguration =
    parameters.getOrElse(throw new IllegalStateException("Parameters not set"))

  def setParameters(params: InputConfiguration): Unit =
    parameters = Some(params)

  def getAskRef: Option[ActorRef[GuardianProtocol.StopVoronoiSCAN]] = askRef

  def setAskRef(ref: Option[ActorRef[GuardianProtocol.StopVoronoiSCAN]]): Unit =
    askRef = ref

  def setWorkerRefs(refs: Set[ActorRef[WorkerProtocol.WorkerRequest]]): Unit =
    workerRefs = refs

  def getWorkers: Array[ActorRef[WorkerProtocol.WorkerRequest]] =
    if (workerRefs.nonEmpty) workerRefs.toArray
    else throw new IllegalStateException("No worker references available")

  def markForExecution(): Unit = shouldStartExecution = true

  def markExecuting(): Unit = isExecuting = true

  def executionConditionsMet: Boolean =
    shouldStartExecution && !isExecuting && workerRefs.size == SystemConfiguration.get.numWorkers

}
