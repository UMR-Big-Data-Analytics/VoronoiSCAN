package metrics

import scala.collection.mutable

object MutualInformation {

  def normalizedMutualInfoScore(labelsTrue: Seq[Int], labelsPred: Seq[Int]): Double = {
    val mi    = mutualInfoScore(labelsTrue, labelsPred)
    val hTrue = entropy(labelsTrue)
    val hPred = entropy(labelsPred)
    val eps = 1e-12
    if (math.abs(hTrue) <= eps && math.abs(hPred) <= eps) 1.0
    else if (math.abs(hTrue) <= eps || math.abs(hPred) <= eps) 0.0
    else mi / math.sqrt(hTrue * hPred)
  }

  private def entropy(labels: Seq[Int]): Double = {
    val labelCounts = labels.groupBy(identity).view.mapValues(_.size.toDouble).toMap
    val total       = labels.size.toDouble
    labelCounts.values.foldLeft(0.0) { (h, count) =>
      val prob = count / total
      h - prob * math.log(prob)
    }
  }

  def mutualInfoScore(labelsTrue: Seq[Int], labelsPred: Seq[Int]): Double = {
    require(labelsTrue.length == labelsPred.length, "Input sequences must have the same length")

    val pairCounts = mutable.Map[(Int, Int), Int]()
    labelsTrue.zip(labelsPred).foreach { case (trueLabel, predLabel) =>
      pairCounts((trueLabel, predLabel)) = pairCounts.getOrElse((trueLabel, predLabel), 0) + 1
    }

    val trueCounts = mutable.Map[Int, Int]()
    val predCounts = mutable.Map[Int, Int]()
    labelsTrue.foreach(label => trueCounts(label) = trueCounts.getOrElse(label, 0) + 1)
    labelsPred.foreach(label => predCounts(label) = predCounts.getOrElse(label, 0) + 1)

    val n = labelsTrue.length.toDouble

    pairCounts.foldLeft(0.0) { case (mi, ((trueLabel, predLabel), count)) =>
      val jointProb = count / n
      val trueProb  = trueCounts(trueLabel) / n
      val predProb  = predCounts(predLabel) / n
      mi + jointProb * math.log(jointProb / (trueProb * predProb))
    }
  }

}
