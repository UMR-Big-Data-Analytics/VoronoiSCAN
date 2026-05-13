package api

import actors.Guardian
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorSystem, Scheduler}
import akka.util.Timeout
import configuration.{InputConfiguration, SystemConfiguration}
import data.Point.Embedding
import io.{CSVReader, CSVWriter}
import protocol._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class VoronoiSCANAkka(parametersOption: Option[InputConfiguration] = None) extends AbstractVoronoiSCAN {

  override def fitTransform(x: Array[Embedding]): Array[Int] = fitTransform(x, 3.minutes)

  def fitTransform(x: Array[Embedding], executionTimeout: Timeout): Array[Int] = {
    if (parametersOption.isEmpty) {
      throw new IllegalArgumentException("InputConfiguration must be provided.")
    }
    val parameters = parametersOption.get
    val writer     = new CSVWriter("/tmp/points.csv")
    writer.write(x)
    val params = InputConfiguration(
      inputPath = "/tmp/points.csv", epsilon = parameters.epsilon, minPts = parameters.minPts,
      numCells = parameters.numCells, minCellDistanceFactor = parameters.minCellDistanceFactor,
      batchSize = parameters.batchSize, labelOutput = parameters.labelOutput, metricsPath = parameters.metricsPath,
      sampler = parameters.sampler, dbscanImpl = parameters.dbscanImpl, seed = parameters.seed
    )

    fitTransform(params, executionTimeout)
  }

  def fitTransform(params: InputConfiguration, executionTimeout: Timeout = 3.minutes): Array[Int] = {
    val system =
      ActorSystem(Guardian(params), SystemConfiguration.get.actorSystemName, SystemConfiguration.get.toAkkaConfig)

    implicit val timeout: Timeout     = executionTimeout
    implicit val scheduler: Scheduler = system.scheduler
    import system.executionContext
    val futureLabels: Future[GuardianProtocol.StopVoronoiSCAN] =
      system.ask(ref => GuardianProtocol.ExecuteVoronoiSCAN(Some(ref)))

    val labels = Await.result(
      futureLabels.map {
        case GuardianProtocol.StopVoronoiSCAN(Some(labels)) => labels
        case GuardianProtocol.StopVoronoiSCAN(None)         => Array.empty[Int]
      },
      timeout.duration
    )

    system.terminate()

    labels
  }

}

object VoronoiSCANAkkaDemo {

  def main(args: Array[String]): Unit = {
    val epsilon = 25
    val minPts  = 3
    val x       = new CSVReader("data/icml/icml.csv").read
    val parameters = InputConfiguration(
      epsilon = epsilon, minPts = minPts, numCells = 25, minCellDistanceFactor = 2, batchSize = 100,
      inputPath = "data/icml/icml.csv", labelOutput = "memory", metricsPath = "", sampler = "random",
      dbscanImpl = "grid", seed = 42
    )
    val voronoiScan = new VoronoiSCANAkka(Some(parameters))
    val labels      = voronoiScan.fitTransform(x)
    require(labels.length == x.length, "Number of labels does not match the number of points")
    println(labels.mkString(", "))
  }

}
