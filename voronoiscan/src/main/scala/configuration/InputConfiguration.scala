package configuration

import singletons.AbstractGenericSingleton

object InputConfiguration extends AbstractGenericSingleton[InputConfiguration]() {

  set(empty)

  def empty: InputConfiguration =
    InputConfiguration(
      epsilon = 0.0f, minPts = 0, inputPath = "", numCells = 0, minCellDistanceFactor = 0.0f, batchSize = 0,
      labelOutput = "ignored", metricsPath = "", sampler = "random", dbscanImpl = "grid", seed = 42
    )

}

case class InputConfiguration(
    epsilon: Float,
    minPts: Int,
    inputPath: String,
    numCells: Int,
    minCellDistanceFactor: Float,
    batchSize: Int,
    numPartitioners: Int = 1,
    labelOutput: String,
    metricsPath: String,
    sampler: String,
    dbscanImpl: String,
    seed: Int
) {

  def update(command: MasterCommand): Unit = {
    val copy = this.copy(
      epsilon = Option(command.epsilon).getOrElse(this.epsilon),
      minPts = Option(command.minPts).getOrElse(this.minPts),
      inputPath = Option(command.inputPath).getOrElse(this.inputPath),
      numCells = Option(command.numCells).getOrElse(this.numCells),
      minCellDistanceFactor = Option(command.minCellDistanceFactor).getOrElse(this.minCellDistanceFactor),
      batchSize = Option(command.batchSize).getOrElse(this.batchSize),
      numPartitioners = Option(command.numPartitioners).getOrElse(this.numPartitioners),
      labelOutput = Option(command.labelOutput).getOrElse(this.labelOutput),
      metricsPath = Option(command.metricsPath).getOrElse(this.metricsPath),
      sampler = Option(command.sampler).getOrElse(this.sampler),
      dbscanImpl = Option(command.dbscanImpl).getOrElse(this.dbscanImpl),
      seed = Option(command.seed).getOrElse(this.seed)
    )
    InputConfiguration.set(copy)
  }

}
