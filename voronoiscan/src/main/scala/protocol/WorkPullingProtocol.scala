package protocol

import akka.actor.typed.ActorRef

sealed trait WorkPullerMessage[+T]

@SerialVersionUID(202506020022L)
final case class Assignment[T](task: T) extends WorkPullerMessage[T]

@SerialVersionUID(202506020023L)
case object Dismiss extends WorkPullerMessage[Nothing]

sealed trait CoordinatorProtocol[T, R]

@SerialVersionUID(202506020021L)
final case class TaskCompleted[T, R](result: R, worker: ActorRef[WorkPullerMessage[T]])
    extends CoordinatorProtocol[T, R]
