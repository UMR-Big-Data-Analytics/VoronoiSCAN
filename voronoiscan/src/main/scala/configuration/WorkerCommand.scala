package configuration

import com.beust.jcommander.{Parameter, Parameters}

@Parameters(commandDescription = "Start a worker ActorSystem.")
class WorkerCommand extends Command {

  @Parameter(names = Array("-mh", "--masterhost"), description = "The host name or IP of the master", required = false)
  var masterhost: String = SystemConfiguration.get.host

  @Parameter(names = Array("-mp", "--masterport"), description = "The port of the master", required = false)
  var masterport: Int = SystemConfiguration.DEFAULT_MASTER_PORT

  def getDefaultPort: Int = SystemConfiguration.DEFAULT_WORKER_PORT

}
