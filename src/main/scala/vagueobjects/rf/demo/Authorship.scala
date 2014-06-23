package vagueobjects.rf.demo

import scala.collection.immutable
import scala.io.Source
import vagueobjects.rf.variables.{FixedSampleSelector, Variable}
import scala.collection.mutable.ListBuffer
import vagueobjects.rf.train._
import vagueobjects.rf.construction._
import vagueobjects.rf.ForestSpecifics
import vagueobjects.rf.variables.OrdinalVariable
import scala.Some
import java.io.FileInputStream
import vagueobjects.rf.variables.OrdinalVariable
import scala.Some

object Authorship extends App{
  val (forestSize,  nbrWorkers, path, nbrTry )  = (args(0).toInt, args(1).toInt, args(2), args(3).toInt)

  val (data,vars) = makeInput(path)


  val af = new ParallelForestBuilder(  nbrWorkers )   with Specs
  af.growForest()


  def makeInput(path:String  ) = {

    val trainingData  = new InMemoryStore

    var labelMap = new immutable.HashMap[String,Int]

    val io = new FileInputStream(path)
    val source = Source.fromInputStream(io, "UTF-8").getLines()
    val columnDef = source.next().split(",")
    val numCols = columnDef.length

    while(source.hasNext){
      val line = source.next().split(",")
      val author = line(numCols-1)
      val labelId = labelMap.get(author) match {
        case Some(a) => a
        case None =>
          val id  = labelMap.size
          labelMap += (author->id)
          id

      }
      val row = new Array[Int](numCols - 2)
      for(i <- 0 until numCols -2){
        row(i) = line(i).toInt
      }
      trainingData.append( row  , labelId)
    }

    trainingData.commit()
    io.close()
    val ordVars = (0  until numCols- 2).map(i => new OrdinalVariable(i))
    val vs = new ListBuffer[Variable] ++ ordVars
    val variables = vs.toList
    (trainingData, variables)
  }

  trait Specs extends ForestSpecifics
      with SamplerProvider
      with DataSourceProvider
      with OutOfBagTestStrategy
      with DefaultTreeFinalizer {
    val varSelector = new FixedSampleSelector(nbrTry)
    val trainData = data
    val testData =  null
    val variables:List[Variable] = vars
    val forestSize = Authorship.forestSize
    val sampleStrategy = BootstrapSampleStrategy
    val outputPath = null
  }

}
