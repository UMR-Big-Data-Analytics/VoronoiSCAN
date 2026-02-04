package configuration

import com.beust.jcommander.{Parameter, Parameters}

@Parameters(commandDescription = "Start a master ActorSystem.")
class MasterCommand extends Command {

  @Parameter(
    names = Array("-i", "--inputPath"),
    description = "Input path for the input data; all files in this folder are considered",
    required = true,
    arity = 1
  )
  var inputPath: String = InputConfiguration.get.inputPath

  @Parameter(names = Array("-e", "--epsilon"), description = "Epsilon parameter for clustering", required = true)
  var epsilon: Float = _

  @Parameter(names = Array("-m", "--minPts"), description = "Minimum number of points for a cluster", required = true)
  var minPts: Int = _

  @Parameter(names = Array("-c", "--numCells"), description = "Number of cells for partitioning", required = true)
  var numCells: Int = _

  @Parameter(names = Array("-f", "--minCellDistanceFactor"), description = "Minimum cell distance", required = true)
  var minCellDistanceFactor: Float = _

  @Parameter(names = Array("-b", "--batchSize"), description = "Batch size for processing")
  var batchSize: Int = 10000

  @Parameter(names = Array("-P", "--numPartitioners"), description = "Number of partitioners")
  var numPartitioners: Int = 1

  @Parameter(
    names = Array("-w", "--numWorkers"),
    description = "The number of workers (indexers/validators) to wait for connection until start; should be at least one if the algorithm is started standalone (otherwise there are no workers to run the discovery)",
    required = false
  )
  var numWorkers: Int = SystemConfiguration.get.numWorkers

  @Parameter(
    names = Array("-s", "--sink"),
    description = "Sink to store the labels; either 'memory', 'csv' or 'ignored'"
  )
  var labelOutput: String = "ignored"

  def getDefaultPort: Int = SystemConfiguration.DEFAULT_MASTER_PORT

  @Parameter(
    names = Array("-M", "--metricsPath"),
    description = "Path to store the metrics output"
  )
  var metricsPath: String = ""

  @Parameter(
    names = Array("-S", "--sampler"),
    description = "Sampler to use for selecting cell centers; either 'grid' or 'random'. Default 'random"
  )
  var sampler: String = "random"

  @Parameter(
    names = Array("-D", "--dbscan"),
    description = "Implementation of the local DBSCAN to use; either 'standard', 'grid' or 'grid-actor'. Default 'grid'"
  )
  var dbscanImpl: String = "grid"

  @Parameter(
    names = Array("--seed"),
    description = "Random seed for sampling (only used with random sampler)"
  )
  var seed: Int = -1

}
