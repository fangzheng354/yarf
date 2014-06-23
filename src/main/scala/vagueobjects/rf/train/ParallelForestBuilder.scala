package vagueobjects.rf.train

import akka.actor._
import scala.collection.mutable.ListBuffer
import akka.routing.RoundRobinRouter
import java.util.UUID
import vagueobjects.rf.construction.{DataSourceProvider, SamplerProvider,TreeBuilder}
import vagueobjects.rf.ForestSpecifics

class ParallelForestBuilder ( nbrWorkers:Int )  {

  this:ForestSpecifics  with SamplerProvider with DataSourceProvider with PostProcessing =>

  import RFMessage._

  class Master(listener: ActorRef) extends Actor with ActorLogging{

    var nbrJobsComplete = 0
    var nbrReceivedVotes = 0

    var aggregation: Array[ListBuffer[ Int ]] = _

    val workerRouter = context.actorOf(
      Props(new Worker ( ))
        .withRouter(RoundRobinRouter(nbrWorkers)), name = "workerRouter")

    def receive = {
      case GrowForest =>
        for (i <- 0 until nbrWorkers)  workerRouter ! GrowTrees( forestSize / nbrWorkers )

      case PartialResult(predictions)  =>
        nbrJobsComplete +=1
        log.info(s"Job #$nbrJobsComplete is done")

        if(aggregation == null){
          aggregation = predictions
        } else {
          for(r<-0 until aggregation.length){
            aggregation(r) ++= predictions (r)
          }
        }

        if(nbrJobsComplete == nbrWorkers){
          log.info("forest is trained, now testing")

          val error  =  aggregateVotes(aggregation )
          log.info(s"error: $error")
          listener ! BuildComplete
        }

    }
  }



  class Worker( )  extends Actor with ActorLogging {

    val id = UUID.randomUUID().toString

    def receive = {
      //produces a subset of trees (less than forest)
      case GrowTrees(nbrTrees ) =>
        log info s"growing $nbrTrees trees"

        val builder = new TreeBuilder( trainData, variables, varSelector , sampleStrategy)

        val votes: Array[ListBuffer[ Int ]] = Array.tabulate( trainData.nbrSamples) {
          _ => new ListBuffer[ Int ]
        }

        for(i<-0 until nbrTrees){
          builder.reset()
          val tree = builder.build()
          collectVotes(tree, votes)
          finish(tree)
          log info s"Worker $id completed tree #$i"

        }
        log info s"Worker $id constructed $nbrTrees trees"
        sender ! PartialResult(votes)
    }
  }

  def growForest() {
    val system = ActorSystem("YARF" )
    val listener = system.actorOf(Props(new Listener( )), name = "listener")
    val master = system.actorOf(Props(new Master(listener )), name = "master")
    master ! GrowForest
  }

  class Listener( ) extends Actor with ActorLogging{


    def receive = {

      case BuildComplete  =>
        log.info("done")
        context.system.shutdown()
    }
  }



}