package actors

import actors.Reaper.set
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import protocol.ReaperProtocol
import singletons.AbstractGenericSingleton

import scala.collection.mutable

object Reaper extends AbstractGenericSingleton[ActorRef[ReaperProtocol.ReaperRequest]]() {

  final val DEFAULT_NAME = "reaper"

  def apply(): Behavior[ReaperProtocol.ReaperRequest] =
    new Reaper().behavior

  def watchWithDefaultReaper[T](actorRefToWatch: ActorRef[T]): Unit =
    get ! ReaperProtocol.WatchMeMessage(actorRefToWatch)

}

class Reaper private {

  final private val refs: mutable.Set[ActorRef[_]] = mutable.Set.empty

  private def behavior: Behavior[ReaperProtocol.ReaperRequest] =
    Behaviors.setup { context =>
      set(context.self)
      Behaviors
        .receiveMessage[ReaperProtocol.ReaperRequest] { case ReaperProtocol.WatchMeMessage(actorRef) =>
          context.log.info(s"Now watching actor: ${actorRef.path.name}")
          if (refs.add(actorRef)) {
            context.watch(actorRef)
          }
          Behaviors.same
        }
        .receiveSignal { case (_, Terminated(actorRef)) =>
          context.log.info(s"Stopped watching actor: ${actorRef.path.name}")
          refs.remove(actorRef)
          context.log.info("Still watching {}", refs)
          if (refs.nonEmpty) {
            Behaviors.same
          } else {
            context.log.info("No more actors to watch, stopping Reaper.")
            Reaper.reset()
            context.system.terminate()
            Behaviors.stopped
          }
        }
    }

}
