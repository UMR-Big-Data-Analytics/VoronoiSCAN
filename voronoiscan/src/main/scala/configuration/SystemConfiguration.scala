package configuration

import com.typesafe.config.{Config, ConfigFactory}
import singletons.AbstractGenericSingleton

import java.net.InetAddress
import scala.util.{Failure, Success, Try}

sealed trait ExecutionRole

object ExecutionRole {

  case object Master extends ExecutionRole
  case object Worker extends ExecutionRole

}

object SystemConfiguration extends AbstractGenericSingleton[SystemConfiguration]() {

  set(SystemConfiguration())

  val DEFAULT_MASTER_PORT = 7877

  val DEFAULT_WORKER_PORT = 7879

  private def getDefaultHost: String = {
    Try {
      return InetAddress.getLocalHost().getHostName()
    } match {
      case Failure(exception) => "0.0.0.0"
      case Success(value)     => value
    }
  }

}

case class SystemConfiguration(
    var role: ExecutionRole = ExecutionRole.Master,
    var host: String = SystemConfiguration.getDefaultHost,
    var port: Int = SystemConfiguration.DEFAULT_MASTER_PORT,
    var masterHost: String = SystemConfiguration.getDefaultHost,
    var masterPort: Int = SystemConfiguration.DEFAULT_MASTER_PORT,
    var actorSystemName: String = "voronoiscan",
    var numWorkers: Int = 1
) {

  def update(masterCommand: MasterCommand): Unit = {
    this.role = ExecutionRole.Master
    this.numWorkers = masterCommand.numWorkers
    this.masterHost = masterCommand.host
    this.masterPort = masterCommand.port
    partialUpdate(masterCommand)
  }

  def update(workerCommand: WorkerCommand): Unit = {
    this.role = ExecutionRole.Worker
    this.masterHost = workerCommand.masterhost
    this.masterPort = workerCommand.masterport
    partialUpdate(workerCommand)
  }

  private def partialUpdate(command: Command): Unit = {
    this.host = command.host
    this.port = command.port
  }

  def toAkkaConfig: Config =
    ConfigFactory
      .parseString(
        "" + "akka.remote.artery.canonical.hostname = \"" + this.host + "\"\n" +
          "akka.remote.artery.canonical.port = " + this.port + "\n" +
          "akka.cluster.roles = [" + this.role + "]\n" +
          "akka.cluster.seed-nodes = [\"akka://" + this.actorSystemName + "@" + this.masterHost + ":" + this.masterPort + "\"]"
      )
      .withFallback(ConfigFactory.load("application"))

}
