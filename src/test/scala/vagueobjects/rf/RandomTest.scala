package vagueobjects.rf

import org.scalatest.FunSpec
import vagueobjects.rf.construction._
import vagueobjects.rf.variables._
import vagueobjects.rf.variables.CategoricalVariable
import scala.util.Random

class RandomTest extends FunSpec{

  describe("A super random tree. Allows any path to run through") {
    it("should work with simple selector"){
      buildTree( )
    }

  }

 

  def buildTree(){
    val source = new InMemoryStore
    val vs   = SimpleSelector
    val rnd = new Random
    val size = 30
    //fill it
    val numRows = 100
    Range(0, numRows).map(i=>(randomRow(size, rnd), i % size))
      .foreach {  f => source append (f._1.arr, f._2) }
    source.commit()
    assert(source.nbrSamples==numRows)
    val variables = Range(0, 10).map(i=>new OrdinalVariable(i)).toList

    object S extends SamplerProvider with DataSourceProvider{
      val trainData = source
      val testData = null
      val sampleStrategy = BootstrapSampleStrategy

    }



    var builder = new TreeBuilder(source, variables, vs, S.sampleStrategy)
    val forestSize = 100
    val forest = new Array[RandomTree](forestSize)
    for(t <- 0 until forestSize){
      builder = new TreeBuilder(source, variables, vs, S.sampleStrategy)
      forest(t) = builder.build()
    }
    for(tree <- forest){
      //build 20 random paths
      for(i <- 0 until 20){
        val path = randomRow(size, rnd)
        val leaf = tree.dropDown(path)
        var countZero = 0
        leaf.stats.foreach{i => if(i == 0) countZero +=1}
        assert(countZero == size -1)
      }
    }
  }

 def randomRow(size:Int, rnd:Random ) =   new  ArraySample(Array.ofDim[Int](size).map( _ => rnd.nextInt(10)))

}

