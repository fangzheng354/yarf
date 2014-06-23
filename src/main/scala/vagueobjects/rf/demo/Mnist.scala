package vagueobjects.rf.demo

import vagueobjects.rf.construction._
import java.io.{File, FilenameFilter, FileInputStream}
import bak.pcj.list.IntArrayList
import java.util.Scanner
import vagueobjects.rf.train.{PersistentTreeFinalizer, SimpleTestStrategy, ParallelForestBuilder}
import vagueobjects.rf.variables.FixedSampleSelector
import vagueobjects.rf.ForestSpecifics
import vagueobjects.rf.variables.CategoricalVariable
import scala.util.Random
import scala.collection.mutable.ListBuffer

/**
 * Trains trees from MNIST data set and stores them on disc
 */
object Mnist extends App{
  val nbrFeatures = 784
  val numValues = 256
  val nbrLabels = 10

  val (numRecords, path, outputPath, forestSize, nbrWorkers, nbrTry, trainTestRatio )
    = (args(0).toInt, args(1),  args(2), args(3).toInt, args(4).toInt, args(5).toInt, args(6).toFloat)

  require(forestSize >= nbrWorkers)
  println("extracting data")
  //remove stored trees id any
  clear( )

  val inputStream =  new FileInputStream(path)
  val source = new Scanner(inputStream)

  val trainLabels = new IntArrayList()
  val testLabels = new IntArrayList()
  var trainList = new ListBuffer[Array[Int]]
  var testList = new ListBuffer[Array[Int]]
  var recCount = 0
  val rnd = new Random()
  var trainIndex = new IntArrayList()

  while(source.hasNextLine && recCount< numRecords){
    val line = source.nextLine().split("\\s")
    val label = line(0).toInt
    val choice = rnd.nextFloat()

    if(choice<trainTestRatio){
      trainLabels.add(label)
      val next =  new Array[Int](nbrFeatures+1)
      for( (k,v) <- offLine(line)){
        next(k) = v
      }
      trainList += next

    } else {
      testLabels.add(label)
      val next =  new Array[Int](nbrFeatures+1)
      for( (k,v) <- offLine(line)) {
        next(k) = v
      }
      testList += next
    }
    
    
    if(recCount>0 && recCount % 250000 ==0){
      println(s"passed $recCount records")
    }

    recCount +=1
  }



  trainLabels.trimToSize()
  testLabels.trimToSize()

  val vars = (0 until nbrFeatures).map(k => new CategoricalVariable(k , (0 until numValues).toArray)).toList

  val trainData = new KVStore(trainList.toArray, trainLabels.toArray, nbrLabels, nbrFeatures)

  val testData = new KVStore(testList.toArray, testLabels.toArray, nbrLabels, nbrFeatures)

  println("training now")

  val af = new ParallelForestBuilder( nbrWorkers  ) with Specs
  af.growForest()

  println("all done")

  trait Specs extends ForestSpecifics
    with SamplerProvider
    with DataSourceProvider
    with SimpleTestStrategy
    with PersistentTreeFinalizer {

    val varSelector = new FixedSampleSelector(nbrTry)
    val trainData = Mnist.trainData
    val testData = Mnist.testData
    val variables  = Mnist.vars
    val forestSize = Mnist.forestSize
    val sampleStrategy = SimpleSamplingStrategy
    val outputPath = Mnist.outputPath
  }

  def offLine (line:Array[String])=
    for(linePos <-1 until line.length;
        spl = line(linePos).split(":"))
            yield(spl(0).toInt, spl(1).toInt)

  def clear() {
    val outDir = new java.io.File(outputPath)
    if(outDir.isDirectory){
      val stored = outDir.listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = name.endsWith(".ser")
      })
      for(f <- stored){
        f.delete()
      }
    }
  }
}

