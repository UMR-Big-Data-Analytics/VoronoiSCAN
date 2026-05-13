package metrics

import scala.collection.mutable

object MutualInformation {

  // Example usage
  def main(args: Array[String]): Unit = {
    val labelsTrue = Seq(0, 0, 1, 1, 2, 2)
    val labelsPred = Seq(1, 1, 0, 0, 2, 2)

    val miScore = mutualInfoScore(labelsTrue, labelsPred)
    val nmiScore = normalizedMutualInfoScore(labelsTrue, labelsPred)

    println(f"Mutual Information Score: $miScore%.4f")
    println(f"Normalized Mutual Information Score: $nmiScore%.4f")
  }

  // Compute normalized mutual information score
  def normalizedMutualInfoScore(labelsTrue: Seq[Int], labelsPred: Seq[Int]): Double = {
    val mi    = mutualInfoScore(labelsTrue, labelsPred)
    val hTrue = entropy(labelsTrue)
    val hPred = entropy(labelsPred)
    val eps = 1e-12
    // If both entropies are essentially zero (both constant assignments) consider them perfectly matched
    // This is different to normal mutual info where MI would be 0 in this case. Here, NMI is defined to be 1.
    // As both contain no information, they are perfectly matched.
    if (math.abs(hTrue) <= eps && math.abs(hPred) <= eps) 1.0
    // If only one has zero entropy, there's no shared information
    else if (math.abs(hTrue) <= eps || math.abs(hPred) <= eps) 0.0
    else mi / math.sqrt(hTrue * hPred)
  }

  // Compute the entropy of a label sequence
  private def entropy(labels: Seq[Int]): Double = {
    val labelCounts = labels.groupBy(identity).view.mapValues(_.size.toDouble).toMap
    val total       = labels.size.toDouble
    labelCounts.values.foldLeft(0.0) { (h, count) =>
      val prob = count / total
      h - prob * math.log(prob)
    }
  }

  // Compute the mutual information score between two discrete variables
  def mutualInfoScore(labelsTrue: Seq[Int], labelsPred: Seq[Int]): Double = {
    require(labelsTrue.length == labelsPred.length, "Input sequences must have the same length")

    // Get counts for each unique pair (true, pred)
    val pairCounts = mutable.Map[(Int, Int), Int]()
    labelsTrue.zip(labelsPred).foreach { case (trueLabel, predLabel) =>
      pairCounts((trueLabel, predLabel)) = pairCounts.getOrElse((trueLabel, predLabel), 0) + 1
    }

    // Marginal counts
    val trueCounts = mutable.Map[Int, Int]()
    val predCounts = mutable.Map[Int, Int]()
    labelsTrue.foreach(label => trueCounts(label) = trueCounts.getOrElse(label, 0) + 1)
    labelsPred.foreach(label => predCounts(label) = predCounts.getOrElse(label, 0) + 1)

    // Total number of samples
    val n = labelsTrue.length.toDouble

    // Calculate mutual information
    pairCounts.foldLeft(0.0) { case (mi, ((trueLabel, predLabel), count)) =>
      val jointProb = count / n
      val trueProb  = trueCounts(trueLabel) / n
      val predProb  = predCounts(predLabel) / n
      mi + jointProb * math.log(jointProb / (trueProb * predProb))
    }
  }

}
