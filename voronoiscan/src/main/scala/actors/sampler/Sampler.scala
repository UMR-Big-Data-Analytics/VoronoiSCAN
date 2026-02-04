package actors.sampler

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import protocol.OrchestratorProtocol.OrchestratorRequest
import protocol.{OrchestratorProtocol, SamplerProtocol}

object Sampler {

  final val DEFAULT_NAME: String = "Sampler"

  def apply(
      orchestrator: ActorRef[OrchestratorRequest],
      cellSampler: CellSampler
  ): Behavior[SamplerProtocol.SamplerRequest] =
    Behaviors.setup { context =>
      new Sampler(context, orchestrator, cellSampler).behavior
    }

}

class Sampler private (
    context: ActorContext[SamplerProtocol.SamplerRequest],
    orchestrator: ActorRef[OrchestratorRequest],
    sampler: CellSampler
) {

  def behavior: Behavior[SamplerProtocol.SamplerRequest] =
    Behaviors.receiveMessage {
      case SamplerProtocol.ExtractSample(numSamples, minDistance, filePath) =>
        context.log.info(s"Start sampling with sampler: $sampler")
        /*val points = Array(
        Point(Array(1.8431633f, 0.9827212f), 38656L),
        Point(Array(3.2590895f, 1.4199113f), 43648L),
        Point(Array(1.9678053f, 1.8907486f), 82048L),
        Point(Array(1.7166355f, 1.2539129f), 17927L),
        Point(Array(0.3983478f, 1.4442424f), 94985L),
        Point(Array(2.207167f, 1.0183074f), 19596L),
        Point(Array(2.880622f, 1.7095879f), 36877L),
        Point(Array(1.3165708f, 1.3963833f), 87180L),
        Point(Array(1.9742979f, 2.6800723f), 57103L),
        Point(Array(2.115577f, 1.2463642f), 58001L),
        Point(Array(2.9892156f, 1.6105843f), 30871L),
        Point(Array(2.8109374f, 1.0668144f), 28056L),
        Point(Array(2.7285705f, 1.8575884f), 39576L),
        Point(Array(0.30057654f, 1.4818385f), 91931L),
        Point(Array(1.5487719f, 2.1362922f), 46364L),
        Point(Array(3.1403172f, 1.1656002f), 19614L),
        Point(Array(1.9026338f, 2.3837683f), 82595L),
        Point(Array(2.2482152f, 2.0563238f), 45731L),
        Point(Array(1.6040893f, 1.3712959f), 59434L),
        Point(Array(2.564314f, 1.4848005f), 71339L),
        Point(Array(1.233174f, 1.6004976f), 82731L),
        Point(Array(2.2564495f, 0.4590019f), 31532L),
        Point(Array(1.8842131f, 0.5534837f), 36911L),
        Point(Array(1.9232726f, 1.231789f), 63663L),
        Point(Array(1.5300204f, 1.8725882f), 72755L),
        Point(Array(1.8273008f, 1.4984027f), 61620L),
        Point(Array(0.94350094f, 1.8498785f), 46901L),
        Point(Array(2.4194777f, 0.15296014f), 27574L),
        Point(Array(1.7990942f, 2.478311f), 63286L),
        Point(Array(1.2895284f, 2.0807083f), 48056L),
        Point(Array(1.258129f, 1.4922359f), 83385L),
        Point(Array(1.7641137f, 0.42509666f), 36025L),
        Point(Array(2.6944804f, 1.107751f), 27707L),
        Point(Array(2.598017f, 1.5956324f), 37435L),
        Point(Array(2.4972534f, 0.44343385f), 38460L),
        Point(Array(0.31391537f, 1.7920549f), 94016L),
        Point(Array(3.2712681f, 0.9618469f), 3141L),
        Point(Array(3.0169191f, 0.34417987f), 17221L),
        Point(Array(2.0351171f, 2.1496365f), 62792L),
        Point(Array(2.437169f, 1.045989f), 17737L),
        Point(Array(3.1688995f, 0.9467561f), 43465L),
        Point(Array(2.4802353f, 1.6926023f), 46030L),
        Point(Array(2.7223496f, 0.8016641f), 35791L),
        Point(Array(1.9089991f, 1.9982988f), 78289L),
        Point(Array(3.186859f, 1.6365569f), 12625L),
        Point(Array(2.7668943f, 0.38160294f), 30557L),
        Point(Array(2.280469f, 1.7144501f), 69724L),
        Point(Array(1.3875437f, 1.7947118f), 68321L),
        Point(Array(1.1398846f, 1.8479626f), 69985L),
        Point(Array(0.909012f, 2.5860786f), 90336L),
        Point(Array(2.2400267f, 0.9210783f), 53990L),
        Point(Array(2.6249886f, 0.40092406f), 37100L),
        Point(Array(1.4518611f, 2.4140368f), 74989L),
        Point(Array(1.8964006f, 1.5893984f), 29165L),
        Point(Array(1.0958183f, 2.609124f), 90860L),
        Point(Array(1.7534388f, 1.0648693f), 20976L),
        Point(Array(2.1842582f, 2.5972443f), 75250L),
        Point(Array(0.7996208f, 1.5631086f), 81778L),
        Point(Array(1.6987581f, 2.0955203f), 58742L),
        Point(Array(1.1260965f, 1.0572557f), 75127L),
        Point(Array(2.1188755f, 2.0206559f), 66554L),
        Point(Array(0.7468763f, 1.928272f), 97402L),
        Point(Array(2.037156f, 1.0561961f), 15103L),
        Point(Array(1.9692667f, 2.4670963f), 48383L),
        )
        val kdTree = new KDTree(points).build()*/
        val (points, kdTree, graph) = sampler.getCenters(filePath, numSamples, minDistance)

        /*val file = new java.io.File(filePath)
        val lines =  Using.resource(scala.io.Source.fromFile(file, "UTF-8")) { source =>
          source.getLines().drop(1).toArray
        }
        val (points, kdTree) = extractSamples(lines, numSamples, minDistance)*/
        orchestrator ! OrchestratorProtocol.ExtractedSamples(points, kdTree, graph)
        Behaviors.same
      case _ =>
        Behaviors.same
    }

}
