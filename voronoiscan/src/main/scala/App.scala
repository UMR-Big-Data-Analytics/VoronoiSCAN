import actors.Guardian
import akka.actor.typed.ActorSystem
import akka.cluster.typed.Cluster
import configuration.ExecutionRole.Master
import configuration.{Command, InputConfiguration, SystemConfiguration}
import org.slf4j.{Logger, LoggerFactory}
import protocol.GuardianProtocol

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object App {

  val logger: Logger = LoggerFactory.getLogger(App.getClass)

  def main(args: Array[String]): Unit = {
    Command(args)
    val systemConfig = SystemConfiguration.get
    val system       = ActorSystem(Guardian(InputConfiguration.get), systemConfig.actorSystemName, systemConfig.toAkkaConfig)
    val cluster = Cluster(system)
    logger.info(s"Actor system '${systemConfig.actorSystemName}' started with role '${systemConfig.role}'.")

    if (systemConfig.role == Master) {
      system ! GuardianProtocol.ExecuteVoronoiSCAN(None)
    }

    Await.result(system.whenTerminated, Duration.Inf)
  }

}
