package actors.dbscan

import akka.actor.typed.{ActorRef, Behavior, DispatcherSelector}
import akka.actor.typed.scaladsl.Behaviors
import protocol.{Assignment, CoordinatorProtocol, TaskCompleted, WorkPullerMessage}

object WorkPulling {

  def coordinator[T, R](
      tasks: IndexedSeq[T],
      numWorkers: Int,
      workerName: String,
      workerFactory: ActorRef[CoordinatorProtocol[T, R]] => Behavior[WorkPullerMessage[T]],
      onResult: R => Unit,
      onComplete: () => Unit,
      dispatcher: DispatcherSelector
  ): Behavior[CoordinatorProtocol[T, R]] = Behaviors.setup { context =>
    val total = tasks.length

    if (total == 0) {
      onComplete()
      Behaviors.stopped
    } else {
      var nextTask  = 0
      var completed = 0

      val workerCount = math.max(1, math.min(numWorkers, total))
      var w           = 0
      while (w < workerCount) {
        val worker = context.spawn(workerFactory(context.self), s"$workerName-$w", dispatcher)
        worker ! Assignment(tasks(nextTask))
        nextTask += 1
        w        += 1
      }

      Behaviors.receiveMessage { case TaskCompleted(result, worker) =>
        onResult(result)
        completed += 1

        if (nextTask < total) {
          worker ! Assignment(tasks(nextTask))
          nextTask += 1
        }

        if (completed == total) {
          onComplete()
          Behaviors.stopped
        } else {
          Behaviors.same
        }
      }
    }
  }

}
