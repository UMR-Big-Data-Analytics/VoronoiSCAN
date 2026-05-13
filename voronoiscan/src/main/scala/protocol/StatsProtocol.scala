package protocol

import protocol.Message.CborMessage

import java.time.Instant

object StatsProtocol {

  trait StatsRequest extends CborMessage

  case class ReportTotalStartTime(startTime: Instant) extends StatsRequest

  case class ReportTotalEndTime(endTime: Instant) extends StatsRequest

  case class ReportDelaunayComputationTime(
                                            startTime: Instant,
                                            endTime: Instant,
                                            numPoints: Long,
                                            numDimensions: Int,
                                            cpuTimeMs: Long
                                          ) extends StatsRequest

  case class ReportPartitioningTime(
                                     startTime: Instant,
                                     endTime: Instant,
                                     numPoints: Long,
                                     cpuTimeMs: Long,
                                     sentNetworkPoints: Long
                                   ) extends StatsRequest

  case class ReportEpsilonExtensionTime(
      clustererId: Int,
      startTime: Instant,
      endTime: Instant,
      cpuTimeMs: Long,
      numPointsSend: Int,
      numPointsReceived: Int,
      numIterations: Int,
      numPointsInCell: Int
  ) extends StatsRequest

  case class ReportEpsilonExtensionGlobalTime(startTime: Instant, endTime: Instant) extends StatsRequest

  case class ReportClusteringTime(
                                   clustererId: Int,
                                   startTime: Instant,
                                   endTime: Instant,
                                   numPoints: Int,
                                   cpuTimeMs: Long
                                 ) extends StatsRequest

  case class ReportEpsilonMergingTime(
      clusterPair: (Int, Int),
      startTime: Instant,
      endTime: Instant,
      cpuTimeMs: Long,
      sentNetworkEdges: Long
  ) extends StatsRequest

  case class ReportGlobalMergingTime(startTime: Instant, endTime: Instant, numLocalMerges: Int, cpuTimeMs: Long)
    extends StatsRequest

  case class ReportLabelingTime(
                                 clustererId: Int,
                                 startTime: Instant,
                                 endTime: Instant,
                                 cpuTimeMs: Long,
                                 sentNetworkPoints: Long
                               ) extends StatsRequest

  case class ReportSystemLoad() extends StatsRequest

  case class Shutdown() extends StatsRequest

}
