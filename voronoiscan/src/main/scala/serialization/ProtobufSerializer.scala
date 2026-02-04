package serialization

import akka.actor.ExtendedActorSystem
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, ActorRefResolver}
import akka.serialization.SerializerWithStringManifest
import clusterer.clusterer.{AddPointsProto => PBAddPointsProto, DistributePointsProto => PBDistributePointsProto, LabelPointsProto => PBLabelPointsProto}
import com.google.protobuf.ByteString
import data.kdtree.{KDNodeProto => PBKDNodeProto, KDTreeProto => PBKDTreeProto}
import data.localclusteringmerge.{LocalClusteringMergeProto => PBLocalClusteringMergeProto, MergeProto => PBMergeProto}
import data.point.{PointCollectionProto => PBPointCollectionProto, PointIdCollectionProto => PBPointIdCollectionProto, PointProto => PBPointProto}
import data.{Point => VSPoint}
import dto.localdbscanresult.{LocalDBSCANResultDtoProto => PBLocalDBSCANResultDtoProto}
import dto.{ClusterMapDto, LocalDBSCANResultDto}
import epsilonmerge.epsilonmerge.{PushDBSCANResultDtoChunkProto => PBPushDBSCANResultDtoChunkProto, PushDBSCANResultDtoProto => PBPushDBSCANResultDtoProto}
import globalmerge.globalmerge.{SendPairwiseMergeResultProto => PBSendPairwiseMergeResultProto}
import guardian.guardian.{StopVoronoiSCANProto => PBStopVoronoiSCANProto}
import orchestrator.orchestrator.{FinishedGlobalMergeProto => PBFinishedGlobalMergeProto}
import partitioner.partitioner.{PartitionPointsProto => PBPartitionPointsProto}
import protocol._
import serialization.LongHelper.{packIntPair, unpackIntPair}
import spatial.index.KDTree
import writer.writer.{LabeledPointsChunkProto => PBWriterLabeledPointsChunkProto}
// Added import for Zstandard compression
import com.github.luben.zstd.Zstd

object LongHelper {

  def packIntPair(a: Int, b: Int): Long =
    (a.toLong << 32) | (b.toLong & 0xffffffffL)

  def unpackIntPair(value: Long): (Int, Int) = {
    val a = (value >>> 32).toInt
    val b = value.toInt
    (a, b)
  }

}

class ProtobufSerializer(system: ExtendedActorSystem) extends SerializerWithStringManifest {

  private val resolver: ActorRefResolver = ActorRefResolver(system.toTyped)

  private val GuardianStop = "guardian.StopVoronoiSCAN"

  private val OrchestratorExtract = "orchestrator.ExtractedSamples"

  private val OrchestratorFinished = "orchestrator.FinishedGlobalMerge"

  private val PartitionerPoints = "partitioner.PartitionPoints"

  private val WriterLabeledChunk = "writer.LabeledPointsChunk"

  private val EpsilonPush = "epsilonmerge.PushDBSCANResult"

  private val EpsilonPushChunk = "epsilonmerge.PushDBSCANResultChunk"

  private val EpsilonRequestResult = "epsilonmerge.RequestDBSCANResult"

  private val ClustererAddPoints = "clusterer.AddPoints"

  private val ClustererDistribute = "clusterer.DistributePoints"

  private val ClustererLabel = "clusterer.LabelPoints"

  private val GlobalMergeSendPair = "globalmerge.SendPairwiseMergeResult"

  // Compression configuration
  private val defaultCompressionThreshold = 512

  private val compressionThreshold: Int =
    try system.settings.config.getInt("voronoiscan.serialization.zstd.compression-threshold")
    catch { case _: Exception => defaultCompressionThreshold }

  private val FlagUncompressed: Byte = 0

  private val FlagCompressed: Byte = 1

  override def identifier: Int = 7712345

  override def manifest(o: AnyRef): String = o match {
    case _: GuardianProtocol.StopVoronoiSCAN            => GuardianStop
    case _: OrchestratorProtocol.ExtractedSamples       => OrchestratorExtract
    case _: OrchestratorProtocol.FinishedGlobalMerge    => OrchestratorFinished
    case _: PartitionerProtocol.PartitionPoints         => PartitionerPoints
    case _: WriterProtocol.LabeledPointsChunk           => WriterLabeledChunk
    case _: EpsilonMergeProtocol.PushDBSCANResult       => EpsilonPush
    case _: EpsilonMergeProtocol.PushDBSCANResultChunk  => EpsilonPushChunk
    case _: ClustererProtocol.AddPoints                 => ClustererAddPoints
    case _: ClustererProtocol.DistributePoints          => ClustererDistribute
    case _: ClustererProtocol.LabelPoints               => ClustererLabel
    case _: GlobalMergeProtocol.SendPairwiseMergeResult => GlobalMergeSendPair
    case other =>
      throw new IllegalArgumentException(s"Unsupported type for Protobuf serialization: ${other.getClass.getName}")
  }

  override def toBinary(o: AnyRef): Array[Byte] = {
    val raw: Array[Byte] = o match {
      case GuardianProtocol.StopVoronoiSCAN(labels) =>
        val flatLabels = labels.getOrElse(Array.empty[Int])
        PBStopVoronoiSCANProto(labels = flatLabels).toByteArray

      case OrchestratorProtocol.FinishedGlobalMerge(globalMergeResult) =>
        val packed: Map[Long, Int] = globalMergeResult.map { case ((a, b), v) => (packIntPair(a, b), v) }
        PBFinishedGlobalMergeProto(globalMergeResult = packed).toByteArray

      case PartitionerProtocol.PartitionPoints(points) =>
        PBPartitionPointsProto(points = points.map(PointConv.toProto)).toByteArray

      case WriterProtocol.LabeledPointsChunk(ids, labels, chunkIdx, totalChunks) =>
        PBWriterLabeledPointsChunkProto(
          ids = ids,
          labels = labels,
          chunkIdx = chunkIdx,
          totalChunks = totalChunks
        ).toByteArray

      case EpsilonMergeProtocol.PushDBSCANResult(resultDto) =>
        PBPushDBSCANResultDtoProto(
          clusteringResult = Some(LocalDBSCANResultDtoConv.toProto(resultDto))
        ).toByteArray

      case EpsilonMergeProtocol.PushDBSCANResultChunk(chunk, chunkIdx, totalChunks, cellIdx) =>
        PBPushDBSCANResultDtoChunkProto(
          chunk = ByteString.copyFrom(chunk),
          chunkIdx = chunkIdx,
          totalChunks = totalChunks,
          cellIdx = cellIdx
        ).toByteArray

      case ClustererProtocol.AddPoints(points) =>
        PBAddPointsProto(points = points.map(PointConv.toProto)).toByteArray

      case ClustererProtocol.DistributePoints(points, senderIdx, replyTo) =>
        val path = resolver.toSerializationFormat(replyTo)
        PBDistributePointsProto(
          points = points.map(PointConv.toProto),
          senderIdx = senderIdx,
          replyTo = path
        ).toByteArray

      case ClustererProtocol.LabelPoints(clusterMapDto) =>
        val (ids, labels) = ClusterMapConv.toPackedIdsAndLabels(clusterMapDto)
        PBLabelPointsProto(ids = ids, labels = labels).toByteArray

      case GlobalMergeProtocol.SendPairwiseMergeResult(localClusteringMerge) =>
        PBSendPairwiseMergeResultProto(
          localClusteringMerge = Some(LocalClusteringMergeConv.toProto(localClusteringMerge))
        ).toByteArray

      case other =>
        throw new IllegalArgumentException(s"Unsupported type for Protobuf serialization: ${other.getClass.getName}")
    }
    maybeCompress(raw)
  }

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = {
    val raw = decompressIfNeeded(bytes)
    manifest match {
      case GuardianStop =>
        val pb = PBStopVoronoiSCANProto.parseFrom(raw)
        GuardianProtocol.StopVoronoiSCAN(Some(pb.labels))

      case OrchestratorFinished =>
        val pb                             = PBFinishedGlobalMergeProto.parseFrom(raw)
        val unpacked: Map[(Int, Int), Int] = pb.globalMergeResult.map { case (k, v) => unpackIntPair(k) -> v }
        OrchestratorProtocol.FinishedGlobalMerge(unpacked)

      case PartitionerPoints =>
        val pb = PBPartitionPointsProto.parseFrom(raw)
        PartitionerProtocol.PartitionPoints(pb.points.map(PointConv.fromProto))

      case WriterLabeledChunk =>
        val pb = PBWriterLabeledPointsChunkProto.parseFrom(raw)
        WriterProtocol.LabeledPointsChunk(pb.ids, pb.labels, pb.chunkIdx, pb.totalChunks)

      case EpsilonPush =>
        val pb = PBPushDBSCANResultDtoProto.parseFrom(raw)
        val dto = pb.clusteringResult
          .map(LocalDBSCANResultDtoConv.fromProto)
          .getOrElse(throw new IllegalArgumentException("Missing clusteringResult in PushDBSCANResultProto"))
        EpsilonMergeProtocol.PushDBSCANResult(dto)

      case EpsilonPushChunk =>
        val pb = PBPushDBSCANResultDtoChunkProto.parseFrom(raw)
        EpsilonMergeProtocol.PushDBSCANResultChunk(pb.chunk.toByteArray, pb.chunkIdx, pb.totalChunks, pb.cellIdx)

      case ClustererAddPoints =>
        val pb = PBAddPointsProto.parseFrom(raw)
        ClustererProtocol.AddPoints(pb.points.map(PointConv.fromProto))

      case ClustererDistribute =>
        val pb                                                    = PBDistributePointsProto.parseFrom(raw)
        val points                                                = pb.points.map(PointConv.fromProto)
        val replyTo: ActorRef[ClustererProtocol.ClustererRequest] = resolver.resolveActorRef(pb.replyTo)
        ClustererProtocol.DistributePoints(points, pb.senderIdx, replyTo)

      case ClustererLabel =>
        val pb            = PBLabelPointsProto.parseFrom(raw)
        val clusterMapDto = ClusterMapConv.fromPackedIdsAndLabels(pb.ids, pb.labels)
        ClustererProtocol.LabelPoints(clusterMapDto)

      case GlobalMergeSendPair =>
        val pb = PBSendPairwiseMergeResultProto.parseFrom(raw)
        val lcm = pb.localClusteringMerge
          .map(LocalClusteringMergeConv.fromProto)
          .getOrElse(throw new IllegalArgumentException("Missing localClusteringMerge in SendPairwiseMergeResultProto"))
        GlobalMergeProtocol.SendPairwiseMergeResult(lcm)

      case other =>
        throw new IllegalArgumentException(s"Unknown manifest for Protobuf deserialization: $other")
    }
  }

  // Compression helpers
  private def maybeCompress(payload: Array[Byte]): Array[Byte] = {
    if (payload.length < compressionThreshold) packUncompressed(payload)
    else {
      val compressed = Zstd.compress(payload)
      if (compressed.length < payload.length) packCompressed(compressed) else packUncompressed(payload)
    }
  }

  private def decompressIfNeeded(bytes: Array[Byte]): Array[Byte] = {
    if (bytes.isEmpty) throw new IllegalArgumentException("Empty byte array")
    val flag = bytes(0)
    val data = java.util.Arrays.copyOfRange(bytes, 1, bytes.length)
    flag match {
      case FlagUncompressed => data
      case FlagCompressed =>
        val size = Zstd.getFrameContentSize(data)
        if (size < 0 || size > Int.MaxValue)
          throw new IllegalArgumentException(s"Invalid compressed frame size: $size")
        val out = new Array[Byte](size.toInt)
        val resultSize = Zstd.decompress(out, data)
        if (resultSize != out.length)
          throw new IllegalStateException(s"Decompressed size mismatch: expected ${out.length}, got $resultSize")
        out
      case other => throw new IllegalArgumentException(s"Unknown compression flag: $other")
    }
  }

  private def packUncompressed(p: Array[Byte]): Array[Byte] = {
    val out = new Array[Byte](1 + p.length)
    out(0) = FlagUncompressed
    System.arraycopy(p, 0, out, 1, p.length)
    out
  }

  private def packCompressed(c: Array[Byte]): Array[Byte] = {
    val out = new Array[Byte](1 + c.length)
    out(0) = FlagCompressed
    System.arraycopy(c, 0, out, 1, c.length)
    out
  }

}

object PointConv {

  def toProto(p: VSPoint): PBPointProto =
    PBPointProto(vector = p.vector, id = p.id, distanceToCenter = Some(p.distanceToCenter))

  def fromProto(pb: PBPointProto): VSPoint = {
    val point = VSPoint(pb.vector.toArray, pb.id)
    pb.distanceToCenter.foreach(d => point.distanceToCenter = d)
    point
  }

}

object KDTreeConv {

  def toProto(tree: KDTree): PBKDTreeProto = {
    PBKDTreeProto(
      points = tree.points.map(PointConv.toProto), root = tree.root.map(KDNodeConv.toProto),
      rebalanceThreshold = tree.rebalanceThreshold, insertions = tree.insertionCount, deletions = tree.deletionCount
    )
  }

  def fromProto(pb: PBKDTreeProto): KDTree = {
    val points = pb.points.map(PointConv.fromProto)
    val depth  = if (pb.depth > 0) pb.depth else 10
    val rbt    = if (pb.rebalanceThreshold > 0) pb.rebalanceThreshold else 100

    // Create tree with new constructor signature
    val tree = new KDTree(initialPoints = points, depth = depth, rebalanceThreshold = rbt)

    // Set the root if it exists
    pb.root.foreach(r => tree.root = Some(KDNodeConv.fromProto(r)))

    // Restore insertion/deletion counts by performing dummy operations
    val targetInsertions = if (pb.insertions > 0) pb.insertions else 0
    val targetDeletions  = if (pb.deletions > 0) pb.deletions else 0

    // We need to set the internal counters through reflection or provide a setter method
    // For now, we'll build the tree and let the counters start at 0
    tree.build()
    tree
  }

  object KDNodeConv {

    def toProto(node: spatial.index.KDNode): PBKDNodeProto =
      PBKDNodeProto(
        pointWithId = Some(PointConv.toProto(node.pointWithId)),
        left = node.left.map(toProto),
        right = node.right.map(toProto)
      )

    def fromProto(pb: PBKDNodeProto): spatial.index.KDNode = {
      val left  = pb.left.map(fromProto)
      val right = pb.right.map(fromProto)
      val point = PointConv.fromProto(
        pb.pointWithId.getOrElse(
          throw new IllegalArgumentException("Missing point_with_id in KDNodeProto")
        )
      )
      spatial.index.KDNode(point, left, right)
    }

  }

}

object ClusterMapConv {

  def toPackedIdsAndLabels(dto: ClusterMapDto): (Array[Long], Array[Int]) = {
    val ids    = new Array[Long](dto.cellIndices.length)
    val labels = new Array[Int](dto.globalClusterIds.length)
    var i      = 0
    while (i < dto.cellIndices.length) {
      ids(i) = packIntPair(dto.cellIndices(i), dto.localClusterIds(i))
      labels(i) = dto.globalClusterIds(i)
      i += 1
    }
    (ids, labels)
  }

  def fromPackedIdsAndLabels(ids: Array[Long], labels: Array[Int]): ClusterMapDto = {
    require(ids.length == labels.length, "ids and labels must have same length")
    val cellIndices     = new Array[Int](ids.length)
    val localClusterIds = new Array[Int](ids.length)
    var i               = 0
    while (i < ids.length) {
      val (cellIdx, localId) = unpackIntPair(ids(i))
      cellIndices(i) = cellIdx
      localClusterIds(i) = localId
      i += 1
    }
    ClusterMapDto(cellIndices, localClusterIds, labels)
  }

}

object LocalDBSCANResultDtoConv {

  def toProto(dto: LocalDBSCANResultDto): PBLocalDBSCANResultDtoProto = {
    PBLocalDBSCANResultDtoProto(
      cellIdx = dto.cellIdx, corePoints = dto.corePoints.map(toCollection),
      borderPoints = dto.borderPoints.map(toCollection), labelKeys = dto.labelKeys, labelValues = dto.labelValues
    )
  }

  private def toCollection(points: Array[VSPoint]): PBPointCollectionProto =
    PBPointCollectionProto(points = points.map(PointConv.toProto))

  private def toCollection(pointIds: Array[Long]): PBPointIdCollectionProto =
    PBPointIdCollectionProto(pointIds = pointIds)

  def fromProto(pb: PBLocalDBSCANResultDtoProto): LocalDBSCANResultDto = {
    LocalDBSCANResultDto(
      cellIdx = pb.cellIdx, corePoints = pb.corePoints.map(fromCollection),
      borderPoints = pb.borderPoints.map(fromCollection), labelKeys = pb.labelKeys, labelValues = pb.labelValues
    )
  }

  private def fromCollection(pb: PBPointCollectionProto): Array[VSPoint] =
    pb.points.map(PointConv.fromProto)

  private def fromCollection(pb: PBPointIdCollectionProto): Array[Long] =
    pb.pointIds

}

object LocalClusteringMergeConv {

  def toProto(lcm: data.LocalClusteringMerge): PBLocalClusteringMergeProto = {
    val merges: Array[PBMergeProto] = lcm.merges.map { case (leftIds, rightIds) =>
      PBMergeProto(leftClusterIds = leftIds.toArray, rightClusterIds = rightIds.toArray)
    }.toArray
    PBLocalClusteringMergeProto(
      leftCellIdx = lcm.leftCellIdx,
      rightCellIdx = lcm.rightCellIdx,
      merges = merges
    )
  }

  def fromProto(pb: PBLocalClusteringMergeProto): data.LocalClusteringMerge = {
    val merges: collection.Set[(collection.Set[Int], collection.Set[Int])] =
      pb.merges.map { m =>
        (m.leftClusterIds.toSet, m.rightClusterIds.toSet)
      }.toSet
    data.LocalClusteringMerge(pb.leftCellIdx, pb.rightCellIdx, merges)
  }

}
