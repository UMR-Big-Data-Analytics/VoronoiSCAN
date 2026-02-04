package actors

import actors.guardian.{CSVPointSink, IgnoredPointSink, MemoryClusteredPointSink}
import akka.actor.typed.{ActorRef, Behavior, ChildFailed}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import configuration.{ExecutionRole, InputConfiguration, SystemConfiguration}
import protocol.{GuardianProtocol, MasterProtocol, WorkerProtocol}

import scala.concurrent.duration.DurationInt

object Guardian {

  final val DEFAULT_NAME = "guardian"

  final val serviceKey: ServiceKey[GuardianProtocol.GuardianRequest] =
    ServiceKey[GuardianProtocol.GuardianRequest](DEFAULT_NAME)

  def apply(config: InputConfiguration): Behavior[GuardianProtocol.GuardianRequest] = Behaviors.setup { context =>
    Behaviors.withTimers(timers => new Guardian(context, timers, config).behavior)
  }

  private class State {

    var master: Option[ActorRef[MasterProtocol.MasterRequest]] = None

    var worker: Option[ActorRef[WorkerProtocol.WorkerRequest]] = None

    var askRef: Option[ActorRef[GuardianProtocol.StopVoronoiSCAN]] = None

    var guardians: Set[ActorRef[GuardianProtocol.GuardianRequest]] = Set.empty

    var executionStarted: Boolean = false

    var startConditionSatisfied: Boolean = false

    var shouldStartExecution: Boolean = false

  }

}

private class Guardian(
    val context: ActorContext[GuardianProtocol.GuardianRequest],
    val timer: TimerScheduler[GuardianProtocol.GuardianRequest],
    val config: InputConfiguration
) {

  import Guardian.State

  private val SHUTDOWN_REATTEMPT_TIMER = "ShutdownReattempt"

  context.log.info("Guardian actor started.")
  context.spawn(Reaper(), Reaper.DEFAULT_NAME)

  context.system.receptionist ! Receptionist.register(Guardian.serviceKey, context.self)

  private val listingResponseAdapter = context.messageAdapter(GuardianProtocol.ReceptionistListingMessage(_))

  context.system.receptionist ! Receptionist.subscribe(Guardian.serviceKey, listingResponseAdapter)

  private val state = new State()

  state.master = if (isMaster) {
    val sink = {
      config.labelOutput match {
        case "memory" => MemoryClusteredPointSink()
        case "csv" =>
          val filePath = config.inputPath + "_labels.csv"
          new CSVPointSink[GuardianProtocol.GuardianRequest](filePath = filePath)
        case "ignored" => IgnoredPointSink()
        case _ => throw new IllegalArgumentException("Unsupported output sink specified. Use 'memory' or 'csv'.")
      }
    }
    context.log.info(s"Waiting for ${SystemConfiguration.get.numWorkers} Workers to connect.")
    val ref = context.spawn(Master(context.self, sink), Master.DEFAULT_NAME)
    Some(ref)
  } else {
    None
  }

  private val workerRef = context.spawn(Worker(context.self), Worker.DEFAULT_NAME)

  state.worker = Some(workerRef)

  def behavior: Behavior[GuardianProtocol.GuardianRequest] = {
    Behaviors
      .receive[GuardianProtocol.GuardianRequest] { (context, message) =>
        message match {
          case GuardianProtocol.ExecuteVoronoiSCAN(askRef) =>
            handleExecuteVoronoiSCAN(askRef)

          case GuardianProtocol.ShutdownMessage(initiator) =>
            handleShutdown(context, message, initiator)

          case GuardianProtocol.ReceptionistListingMessage(listing) =>
            handleReceptionistListing(context, listing)

          case _ =>
            Behaviors.unhandled
        }
      }
      .receiveSignal { case (_, ChildFailed(ref, e)) =>
        context.log.error(
          "Actor {} has terminated. Probably due to some child exception. Shutting down",
          ref.path.name,
          e
        )
        context.self ! GuardianProtocol.ShutdownMessage()
        Behaviors.same
      }
  }

  private def handleReceptionistListing(
      context: ActorContext[GuardianProtocol.GuardianRequest],
      listing: Receptionist.Listing
  ): Behavior[GuardianProtocol.GuardianRequest] = {
    state.guardians = listing.serviceInstances(Guardian.serviceKey)
    if (isMaster) {
      context.log.info(s"Connected ${state.guardians.size}/${SystemConfiguration.get.numWorkers} workers.")
    }
    if (timer.isTimerActive(SHUTDOWN_REATTEMPT_TIMER) && isClusterDown) {
      shutdown()
    }

    Behaviors.same
  }

  private def shutdown(): Unit = {
    state.master match {
      case Some(masterRef) =>
        context.log.info("Shutting down Master actor.")
        masterRef ! MasterProtocol.ShutdownMessage()
        state.master = None
      case None =>
    }
    state.worker match {
      case Some(workerRef) =>
        context.log.info("Shutting down Worker actor.")
        workerRef ! WorkerProtocol.ShutdownMessage()
        state.worker = None
      case None =>
    }
  }

  private def isClusterDown: Boolean =
    state.guardians.isEmpty || (state.guardians.contains(context.self) && state.guardians.size == 1)

  private def isMaster: Boolean = SystemConfiguration.get.role == ExecutionRole.Master

  private def handleShutdown(
      context: ActorContext[GuardianProtocol.GuardianRequest],
      message: GuardianProtocol.GuardianRequest,
      initiator: Option[ActorRef[GuardianProtocol.GuardianRequest]]
  ): Behavior[GuardianProtocol.GuardianRequest] = {
    if (initiator.isDefined || isClusterDown) {
      shutdown()
      Behaviors.same
    } else {
      for (guardian <- state.guardians) {
        if (guardian != context.self) {
          guardian ! GuardianProtocol.ShutdownMessage(Some(context.self))
        }
      }
      if (!timer.isTimerActive(SHUTDOWN_REATTEMPT_TIMER)) {
        this.timer.startTimerAtFixedRate(SHUTDOWN_REATTEMPT_TIMER, message, 5.seconds, 5.seconds)
      }
      Behaviors.same
    }
  }

  private def handleExecuteVoronoiSCAN(
                                        askRef: Option[ActorRef[GuardianProtocol.StopVoronoiSCAN]]
  ): Behavior[GuardianProtocol.GuardianRequest] = {
    state.askRef = askRef
    state.shouldStartExecution = true
    state.master match {
      case Some(masterRef) =>
        masterRef ! MasterProtocol.StartMessage(state.askRef, config)
      case None =>
        throw new IllegalStateException("Master actor is not initialized.")
    }
    Behaviors.same
  }

}
