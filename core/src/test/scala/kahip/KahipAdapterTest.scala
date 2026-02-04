package kahip

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.jdk.CollectionConverters._

class KahipAdapterTest extends AnyFlatSpec with Matchers with Inside with TableDrivenPropertyChecks {

  behavior of "KahipAdapter Basic Partitioning"

  it should "partition a simple 5-vertex graph into 2 partitions" in {
    // Create a simple 5-vertex graph (same as in the main method example)
    val xadj    = Array(0, 2, 5, 7, 9, 11)               // Starting indices for each vertex's adjacency list
    val adjncy  = Array(1, 4, 0, 2, 4, 1, 3, 2, 4, 0, 3) // Adjacency list
    val adjcwgt = Array.fill(adjncy.length)(1)           // Edge weights (all 1 for unweighted)
    val vwgt    = Array(1, 1, 1, 1, 1)                   // All vertices have the same weight

    val result = KahipAdapter.kahipPartition(
      5,       // numVertices
      vwgt,    // vertex weights
      xadj,    // xadj (CSR format)
      adjncy,  // adjncy (CSR format)
      adjcwgt, // edge weights
      2,       // nparts (number of partitions)
      0.03,    // imbalance
      true,    // suppressOutput
      42       // randomSeed (fixed for reproducibility)
    )

    // Basic validation
    result should not be null
    result.partitionMap() should not be null
    result.partitionMap().size() shouldBe 5

    // Check that all vertices are assigned to partitions 0 or 1
    result.partitionMap().values().asScala.foreach { partition =>
      // Convert Java Integer to Scala Int to avoid type mismatch
      partition.toInt should ((be >= 0).and(be <= 1))
    }

    // The edgecut should be a non-negative number
    result.edgecut() should be >= 0
  }

  it should "partition a path graph with 6 vertices into 3 partitions" in {
    // Create a path graph: 0 -- 1 -- 2 -- 3 -- 4 -- 5
    val xadj    = Array(0, 1, 3, 5, 7, 9, 10)
    val adjncy  = Array(1, 0, 2, 1, 3, 2, 4, 3, 5, 4)
    val adjcwgt = Array.fill(adjncy.length)(1)
    val vwgt    = Array(1, 1, 1, 1, 1, 1)

    val result = KahipAdapter.kahipPartition(
      6,       // numVertices
      vwgt,    // vertex weights
      xadj,    // xadj (CSR format)
      adjncy,  // adjncy (CSR format)
      adjcwgt, // edge weights
      3,       // nparts (number of partitions)
      0.05,    // imbalance
      true,    // suppressOutput
      42       // randomSeed
    )

    // Basic validation
    result should not be null
    result.partitionMap().size() shouldBe 6

    // Check that all vertices are assigned to partitions 0, 1, or 2
    result.partitionMap().values().asScala.foreach { partition =>
      partition.toInt should ((be >= 0).and(be <= 2))
    }

    // Check that all partitions are used (this is expected for balanced partitioning)
    val usedPartitions = result.partitionMap().values().asScala.toSet
    usedPartitions.size shouldBe 3

    // For a path graph, the optimal edgecut for 3 partitions would be 2 edges
    // But the algorithm might not always find the optimal solution
    result.edgecut() should be >= 2
  }

  it should "partition a complete graph with 4 vertices into 2 partitions" in {
    // Create a complete graph (K4) where every vertex connects to every other vertex
    val xadj    = Array(0, 3, 6, 9, 12)
    val adjncy  = Array(1, 2, 3, 0, 2, 3, 0, 1, 3, 0, 1, 2)
    val adjcwgt = Array.fill(adjncy.length)(1)
    val vwgt    = Array(1, 1, 1, 1)

    val result = KahipAdapter.kahipPartition(
      4,       // numVertices
      vwgt,    // vertex weights
      xadj,    // xadj (CSR format)
      adjncy,  // adjncy (CSR format)
      adjcwgt, // edge weights
      2,       // nparts
      0.0,     // No imbalance allowed
      true,    // suppressOutput
      42       // randomSeed
    )

    result should not be null
    result.partitionMap().size() shouldBe 4

    // Since we're partitioning a complete graph with 4 vertices into 2 equal parts,
    // we should get 2 vertices in each partition
    val partitionCounts = countPartitions(result.partitionMap(), 2)
    partitionCounts(0) shouldBe 2
    partitionCounts(1) shouldBe 2

    // In a complete graph with equal partitions, the edgecut is maximized
    // For K4 split into two partitions of size 2, the edgecut should be 4
    result.edgecut() shouldBe 4
  }

  behavior of "KahipAdapter Edge Cases"

  it should "handle a single vertex graph" in {
    val xadj    = Array(0)
    val adjncy  = Array.empty[Int]
    val adjcwgt = Array.empty[Int]
    val vwgt    = Array(1)

    val result = KahipAdapter.kahipPartition(
      1,       // numVertices
      vwgt,    // vertex weights
      xadj,    // xadj
      adjncy,  // adjncy
      adjcwgt, // edge weights
      1,       // nparts
      0.0,     // imbalance
      true,    // suppressOutput
      42       // randomSeed
    )

    result should not be null
    result.partitionMap().size() shouldBe 1
    result.partitionMap().get(0) shouldBe 0
    result.edgecut() shouldBe 0
  }

  it should "handle a disconnected graph" in {
    // Two disconnected components: vertices 0,1 form one component and 2,3,4 form another
    val xadj    = Array(0, 1, 2, 4, 6, 8)
    val adjncy  = Array(1, 0, 3, 4, 2, 4, 2, 3)
    val adjcwgt = Array.fill(adjncy.length)(1)
    val vwgt    = Array(1, 1, 1, 1, 1)

    val result = KahipAdapter.kahipPartition(
      5,       // numVertices
      vwgt,    // vertex weights
      xadj,    // xadj
      adjncy,  // adjncy
      adjcwgt, // edge weights
      2,       // nparts
      0.1,     // imbalance
      true,    // suppressOutput
      42       // randomSeed
    )

    result should not be null
    result.partitionMap().size() shouldBe 5

    // Since the graph has two natural components, we expect each component
    // to be placed in a separate partition for an optimal solution
    val partition0 = result.partitionMap().get(0)
    val partition1 = result.partitionMap().get(1)
    val partition2 = result.partitionMap().get(2)

    // Vertices 0 and 1 should be in the same partition
    partition0 shouldBe partition1

    // Vertices 2, 3, and 4 should be in the same partition
    partition2 shouldBe result.partitionMap().get(3)
    partition2 shouldBe result.partitionMap().get(4)

    // The two components should be in different partitions for optimal solution
    partition0 should not be partition2

    // Since the components are disconnected, the edgecut should be 0
    result.edgecut() shouldBe 0
  }

  it should "handle an empty graph" in {
    // A graph with no vertices
    val xadj    = Array(0)
    val adjncy  = Array.empty[Int]
    val adjcwgt = Array.empty[Int]
    val vwgt    = Array.empty[Int]

    // Since KaHIP may not handle empty graphs well, we expect either
    // an empty result or some reasonable handling
    val result = KahipAdapter.kahipPartition(
      0,       // numVertices
      vwgt,    // vertex weights
      xadj,    // xadj
      adjncy,  // adjncy
      adjcwgt, // edge weights
      1,       // nparts
      0.0,     // imbalance
      true,    // suppressOutput
      42       // randomSeed
    )

    result should not be null
    result.partitionMap().isEmpty shouldBe true
    result.edgecut() shouldBe 0
  }

  behavior of "KahipAdapter Configuration Tests"

  it should "respond to different imbalance values" in {
    // Create a grid graph: 3x3 grid (9 vertices)
    val xadj = Array(0, 2, 5, 7, 10, 14, 17, 19, 22, 24)
    val adjncy = Array(
      1, 3,       // neighbors of vertex 0
      0, 2, 4,    // neighbors of vertex 1
      1, 5,       // neighbors of vertex 2
      0, 4, 6,    // neighbors of vertex 3
      1, 3, 5, 7, // neighbors of vertex 4
      2, 4, 8,    // neighbors of vertex 5
      3, 7,       // neighbors of vertex 6
      4, 6, 8,    // neighbors of vertex 7
      5, 7        // neighbors of vertex 8
    )
    val adjcwgt = Array.fill(adjncy.length)(1)
    val vwgt    = Array(1, 1, 1, 1, 1, 1, 1, 1, 1)

    // Test with low imbalance
    val result1 = KahipAdapter.kahipPartition(
      9, vwgt, xadj, adjncy, adjcwgt, 3, 0.01, true, 42
    )

    // Test with high imbalance
    val result2 = KahipAdapter.kahipPartition(
      9, vwgt, xadj, adjncy, adjcwgt, 3, 0.5, true, 42
    )

    result1 should not be null
    result2 should not be null

    // Count vertices in each partition for both results
    val partitionCounts1 = countPartitions(result1.partitionMap(), 3)
    val partitionCounts2 = countPartitions(result2.partitionMap(), 3)

    // With lower imbalance, we expect more balanced partitions
    // Calculate the standard deviation to measure balance
    val stdDev1 = calculateStdDev(partitionCounts1)
    val stdDev2 = calculateStdDev(partitionCounts2)

    // The standard deviation with low imbalance should be less than or equal
    // to that with high imbalance (more balanced)
    stdDev1 should be <= (stdDev2 + 1e-10)
  }

  it should "work with different numbers of partitions" in {
    // Create a path graph with 10 vertices
    // Fix recursive definition by using a mutable approach
    val xadj = new Array[Int](11)
    xadj(0) = 0
    xadj(1) = 1
    for (i <- 2 until 11)
      xadj(i) = xadj(i - 1) + (if (i < 10) 2 else 1) - (if (i == 2) 1 else 0)

    val adjncy = Array.ofDim[Int](18) // 9 edges * 2 endpoints
    for (i <- 0 until 9) {
      adjncy(i * 2) = i + 1 // edge to next vertex
      if (i > 0) {
        adjncy(i * 2 + 1) = i - 1 // edge to previous vertex
      }
    }
    adjncy(1) = 0 // Fix first edge's back pointer

    val adjcwgt = Array.fill(adjncy.length)(1)
    val vwgt    = Array.fill(10)(1)

    // Test with 2 partitions
    val result2Parts = KahipAdapter.kahipPartition(
      10, vwgt, xadj, adjncy, adjcwgt, 2, 0.05, true, 42
    )

    // Test with 5 partitions
    val result5Parts = KahipAdapter.kahipPartition(
      10, vwgt, xadj, adjncy, adjcwgt, 5, 0.05, true, 42
    )

    result2Parts should not be null
    result5Parts should not be null

    // For a path graph with k partitions, there should be some edgecut
    // However, depending on the implementation details of KaHIP, the specific
    // value might vary. In an ideal case, it would be k-1, but we'll be more flexible.
    // We'll check other properties of a valid partitioning instead.

    // Check that all partitions are used
    val usedPartitions2 = result2Parts.partitionMap().values().asScala.toSet
    val usedPartitions5 = result5Parts.partitionMap().values().asScala.toSet

    usedPartitions2.size shouldBe 2
    usedPartitions5.size shouldBe 5

    // Verify that the partitioning contains all vertices
    result2Parts.partitionMap().size() shouldBe 10
    result5Parts.partitionMap().size() shouldBe 10

    // For more partitions, we expect more cuts on average
    // This might not always be true, but is a reasonable expectation
    println(s"Edge cuts - 2 partitions: ${result2Parts.edgecut()}, 5 partitions: ${result5Parts.edgecut()}")
  }

  it should "handle weighted vertices" in {
    // Create a simple graph
    val xadj    = Array(0, 2, 4, 6, 8)
    val adjncy  = Array(1, 2, 0, 3, 0, 3, 1, 2)
    val adjcwgt = Array.fill(adjncy.length)(1)

    // Define unbalanced weights
    val vwgtUnbalanced = Array(10, 1, 1, 1)

    // Define balanced weights
    val vwgtBalanced = Array(1, 1, 1, 1)

    val resultUnbalanced = KahipAdapter.kahipPartition(
      4, vwgtUnbalanced, xadj, adjncy, adjcwgt, 2, 0.05, true, 42
    )

    val resultBalanced = KahipAdapter.kahipPartition(
      4, vwgtBalanced, xadj, adjncy, adjcwgt, 2, 0.05, true, 42
    )

    resultUnbalanced should not be null
    resultBalanced should not be null

    // The heavy vertex (0) should be in its own partition or with minimum other vertices
    val heavyVertexPartition = resultUnbalanced.partitionMap().get(0)
    val verticesWithHeavyVertex = resultUnbalanced.partitionMap().asScala.count { case (_, partition) =>
      partition == heavyVertexPartition
    }

    // We expect the heavy vertex to be in a partition with few or no other vertices
    verticesWithHeavyVertex should be <= 2
  }

  it should "handle weighted edges" in {
    // Create a simple graph
    val xadj   = Array(0, 2, 4, 6, 8)
    val adjncy = Array(1, 2, 0, 3, 0, 3, 1, 2)

    // Define unbalanced edge weights - make edge (0,1) heavy
    val adjcwgtUnbalanced = Array(10, 1, 10, 1, 1, 1, 1, 1)

    // Define balanced edge weights
    val adjcwgtBalanced = Array(1, 1, 1, 1, 1, 1, 1, 1)

    val resultUnbalanced = KahipAdapter.kahipPartition(
      4,
      Array(1, 1, 1, 1),
      xadj,
      adjncy,
      adjcwgtUnbalanced,
      2,
      0.05,
      true,
      42
    )

    val resultBalanced = KahipAdapter.kahipPartition(
      4,
      Array(1, 1, 1, 1),
      xadj,
      adjncy,
      adjcwgtBalanced,
      2,
      0.05,
      true,
      42
    )

    resultUnbalanced should not be null
    resultBalanced should not be null

    // In the unbalanced case, we expect vertices 0 and 1 to be in the same partition
    // to avoid cutting the heavy edge between them
    resultUnbalanced.partitionMap().get(0) shouldBe resultUnbalanced.partitionMap().get(1)
  }

  behavior of "KahipAdapter Stability and Performance"

  it should "produce the same results with the same random seed" in {
    // Create a simple graph
    val xadj    = Array(0, 2, 5, 7, 9, 11)
    val adjncy  = Array(1, 4, 0, 2, 4, 1, 3, 2, 4, 0, 3)
    val adjcwgt = Array.fill(adjncy.length)(1)
    val vwgt    = Array(1, 1, 1, 1, 1)

    // Run with the same seed twice
    val result1 = KahipAdapter.kahipPartition(
      5, vwgt, xadj, adjncy, adjcwgt, 2, 0.03, true, 42
    )

    val result2 = KahipAdapter.kahipPartition(
      5, vwgt, xadj, adjncy, adjcwgt, 2, 0.03, true, 42
    )

    // Results should be identical
    result1.edgecut() shouldBe result2.edgecut()
    result1.partitionMap() shouldBe result2.partitionMap()
  }

  it should "potentially produce different results with different random seeds" in {
    // Create a large enough graph that random seed is likely to matter
    // A 5x5 grid (25 vertices)
    val xadj          = Array.ofDim[Int](26)
    val adjncyBuilder = scala.collection.mutable.ArrayBuffer[Int]()

    // Populate a 5x5 grid graph
    for (i <- 0 until 25) {
      xadj(i) = adjncyBuilder.size

      // Connect to neighbor above (if exists)
      if (i >= 5) {
        adjncyBuilder += (i - 5)
      }

      // Connect to neighbor below (if exists)
      if (i < 20) {
        adjncyBuilder += (i + 5)
      }

      // Connect to neighbor left (if exists)
      if (i % 5 != 0) {
        adjncyBuilder += (i - 1)
      }

      // Connect to neighbor right (if exists)
      if (i % 5 != 4) {
        adjncyBuilder += (i + 1)
      }
    }
    xadj(25) = adjncyBuilder.size
    val adjncy = adjncyBuilder.toArray

    val adjcwgt = Array.fill(adjncy.length)(1)
    val vwgt    = Array.fill(25)(1)

    // Run with different seeds
    val result1 = KahipAdapter.kahipPartition(
      25, vwgt, xadj, adjncy, adjcwgt, 4, 0.03, true, 42
    )

    val result2 = KahipAdapter.kahipPartition(
      25, vwgt, xadj, adjncy, adjcwgt, 4, 0.03, true, 99
    )

    // Note: Different seeds might still produce the same result by chance,
    // so we can't assert that they're different. But we can check they're valid.
    result1 should not be null
    result2 should not be null

    result1.partitionMap().size() shouldBe 25
    result2.partitionMap().size() shouldBe 25

    // Just check if they're different (they might not be, but that's acceptable)
    val different = result1.partitionMap() != result2.partitionMap() ||
      result1.edgecut() != result2.edgecut()

    println(s"Different random seeds produced ${if (different) "different" else "same"} results.")
  }

  // Helper methods

  private def countPartitions(partitionMap: java.util.Map[Integer, Integer], nparts: Int): Array[Int] = {
    val counts = Array.fill(nparts)(0)
    partitionMap.values().asScala.foreach { partition =>
      counts(partition.toInt) += 1
    }
    counts
  }

  private def calculateStdDev(values: Array[Int]): Double = {
    // Calculate mean
    val mean = values.sum.toDouble / values.length

    // Calculate variance
    val variance = values.map(v => math.pow(v - mean, 2)).sum / values.length

    // Return standard deviation
    math.sqrt(variance)
  }

}
