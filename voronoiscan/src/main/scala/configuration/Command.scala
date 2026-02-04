package configuration

import com.beust.jcommander.{JCommander, Parameter, ParameterException}
import configuration.ExecutionRole.{Master, Worker}

import scala.util.{Failure, Success, Try}

object Command {

  def apply(args: Array[String]): Unit = {
    val commandMaster = new MasterCommand()
    val commandWorker = new WorkerCommand()
    val jCommander = JCommander.newBuilder
      .addCommand(Master.toString, commandMaster)
      .addCommand(Worker.toString, commandWorker)
      .build
    Try {
      jCommander.parse(args: _*)
      if (jCommander.getParsedCommand == null) throw new ParameterException("No command given.")
      jCommander.getParsedCommand match {
        case "Master" =>
          SystemConfiguration.get.update(commandMaster)
          InputConfiguration.get.update(commandMaster)

        case "Worker" =>
          SystemConfiguration.get.update(commandWorker)
          InputConfiguration.set(null)

        case _ =>
          throw new AssertionError
      }
    } match {
      case Success(_) =>
      case Failure(e: ParameterException) =>
        System.out.printf("Could not parse args: %s\n", e.getMessage)
        jCommander.usage()
        System.exit(1)
      case Failure(e) =>
        throw e
    }
  }

}

abstract class Command {

  @Parameter(
    names = Array("-h", "--host"),
    description = "This machine's host name or IP that we use to bind this application against",
    required = false
  )
  var host: String = SystemConfiguration.get.host

  @Parameter(
    names = Array("-p", "--port"),
    description = "This machines port that we use to bind this application against",
    required = false
  )
  var port: Int = this.getDefaultPort

  def getDefaultPort: Int

}
