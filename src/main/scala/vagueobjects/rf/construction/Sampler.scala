package vagueobjects.rf.construction

import scala.concurrent.forkjoin.ThreadLocalRandom

trait Sampler {
  def sample( )  : (Array[Int], Array[Int])
}



trait DataSourceProvider {
  val trainData:DataSource
  val testData:DataSource

}

 
trait SamplerProvider {
  this: DataSourceProvider   =>

  val sampleStrategy:Sampler

  object BootstrapSampleStrategy   extends Sampler{

    override def sample( )  = {
      val numRows =   trainData.nbrSamples
      val bag = Array.tabulate(numRows){_ =>   ThreadLocalRandom.current().nextInt(numRows)}

      val unseenIndex = Array.tabulate[Boolean](numRows){i=>true}
      for(i<-bag){
        unseenIndex(i) = false
      }

      val offBag = new Array[Int](numRows)

      for(i<-0 until numRows){
        if(unseenIndex(i)){
          offBag (i) =  1
        }
      }
      (bag, offBag )
    }
  }

  object SimpleSamplingStrategy extends Sampler{
    val numRows =   trainData.nbrSamples
    override def sample( )  =  {
      ((0 until  numRows).toArray , Array.empty[Int])
    }
  }


}