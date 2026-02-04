package metrics

import org.scalactic.Tolerance._
import org.scalatest.funsuite.AnyFunSuite

class MutualInformationTest extends AnyFunSuite {

  test("mutualInfoScore - identical labels") {
    val labelsTrue = Seq(0, 1, 2, 3)
    val labelsPred = Seq(0, 1, 2, 3)
    val expectedMI = math.log(4) // log(4)
    val result     = MutualInformation.mutualInfoScore(labelsTrue, labelsPred)
    assert(result === expectedMI +- 1e-4)
  }

  test("mutualInfoScore - unrelated labels") {
    val labelsTrue = Seq(0, 0, 1, 1)
    val labelsPred = Seq(2, 2, 3, 3)
    val expectedMI = 0.5 * math.log(2) + 0.5 * math.log(2)
    val result     = MutualInformation.mutualInfoScore(labelsTrue, labelsPred)
    assert(result === expectedMI +- 1e-4)
  }

  test("mutualInfoScore - same clustering in different order") {
    val labelsTrue = Seq(0, 0, 1, 1)
    val labelsPred = Seq(1, 1, 0, 0)
    val expectedMI = 0.5 * math.log(2) + 0.5 * math.log(2)
    val result     = MutualInformation.mutualInfoScore(labelsTrue, labelsPred)
    assert(result === expectedMI +- 1e-4)
  }

  test("mutualInfoScore - non-overlapping labels") {
    val labelsTrue = Seq(0, 0, 1, 1)
    val labelsPred = Seq(2, 3, 4, 5)
    val expectedMI = math.log(2)
    val result     = MutualInformation.mutualInfoScore(labelsTrue, labelsPred)
    assert(result === expectedMI +- 1e-4)
  }

  test("normalizedMutualInfoScore - identical labels") {
    val labelsTrue  = Seq(0, 1, 2, 3)
    val labelsPred  = Seq(0, 1, 2, 3)
    val expectedNMI = 1.0
    val result      = MutualInformation.normalizedMutualInfoScore(labelsTrue, labelsPred)
    assert(result === expectedNMI +- 1e-4)
  }

  test("normalizedMutualInfoScore - unrelated labels") {
    val labelsTrue  = Seq(0, 0, 1, 1)
    val labelsPred  = Seq(2, 2, 3, 3)
    val expectedNMI = 1.0 // MI is maximally informative in this case
    val result      = MutualInformation.normalizedMutualInfoScore(labelsTrue, labelsPred)
    assert(result === expectedNMI +- 1e-4)
  }

  test("normalizedMutualInfoScore - different clustering with overlap") {
    val labelsTrue  = Seq(0, 0, 1, 1, 2, 2)
    val labelsPred  = Seq(1, 1, 0, 0, 2, 2)
    val expectedNMI = 1.0 // Identical clustering in a different order
    val result      = MutualInformation.normalizedMutualInfoScore(labelsTrue, labelsPred)
    assert(result === expectedNMI +- 1e-4)
  }

  test("normalizedMutualInfoScore - random independent labels") {
    val labelsTrue = Seq.fill(100)(scala.util.Random.nextInt(5))
    val labelsPred = Seq.fill(100)(scala.util.Random.nextInt(5)) // Independent random labels
    val result     = MutualInformation.normalizedMutualInfoScore(labelsTrue, labelsPred)
    assert(result < 0.2, s"Expected NMI to be close to 0.0 for truly independent random labels, but got $result")
  }

  test("normalizedMutualInfoScore - label invariance") {
    val labelsTrue  = Seq(0, 0, 0, 0)
    val labelsPred  = Seq(1, 1, 1, 1)
    val expectedNMI = 1.0
    val result      = MutualInformation.normalizedMutualInfoScore(labelsTrue, labelsPred)
    assert(result === expectedNMI +- 1e-4)
  }

}
