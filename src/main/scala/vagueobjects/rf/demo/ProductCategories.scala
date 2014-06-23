package vagueobjects.rf.demo

import java.io.{ObjectInputStream, FilenameFilter, File, FileInputStream}
import scala.io.Source
import scala.collection.immutable.{HashSet, HashMap}
import vagueobjects.rf.construction._
import scala.util.Random
import vagueobjects.rf.variables._
import vagueobjects.rf.{RandomTree, ForestSpecifics}
import vagueobjects.rf.train.{ParallelForestBuilder, OutOfBagTestStrategy, PersistentTreeFinalizer}
import java.util
import vagueobjects.rf.variables.CategoricalVariable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object ProductCategories extends App{
  val (path, nbrTry, nbrWorkers, forestSize, outputPath) = (args(0), args(1).toInt,args(2).toInt, args(3).toInt, args(4))
  val MAX_VALUE =5261
  var trainData:MappedStore =_
  var vars: Array[Variable] =_
  val (cats, lineCount, tokenMap) = firstPass()

//  train()
  process()

  def process() {
    val dir = new File(outputPath)

    val treeFiles =   dir.listFiles(new FilenameFilter(){
      def accept(f: File, name: String): Boolean = name.endsWith(".ser")
    })


    for(treeFile <- treeFiles) {
      val inp = new FileInputStream(treeFile)
      val obj = new ObjectInputStream(inp)
      val treeObj: RandomTree = obj.readObject().asInstanceOf[RandomTree]
      scan (treeObj)
//      val itr = new LeafIterator(treeObj)
//      var count = 0
//      while (itr.hasNext){
//        val leaf = itr.next()
//        println(leaf.stats.mkString(","))
//        count += 1
//      }
//      println("************"+ count)
    }
  }


  def train() {


    val source = new Array[Array[Int]](2*lineCount)
    val labels = new Array[Int](2*lineCount)

    var count =0
    //build dictionary
    val input = new FileInputStream(path)
    val src = Source.fromInputStream(input)
    val secondPass = src.getLines()
    while(secondPass.hasNext  ) {
      val line = secondPass.next()
      val separator = line.indexOf(",")
      if(separator>0){
        val catName = line.substring(0, line.indexOf(","))
        val catId = cats.value(catName)

        //generate
        labels(count) = 0

        val tokens = line.substring(separator).split(",").toSet
        val a =    new Array[Int](tokens.size)

        var i = 0
        val next =  new Array[Int](tokens.size)

        for( t<- tokens; if t.length>0) {
          a(i) = tokenMap.value(t)  % MAX_VALUE
          i += 1
        }

        source(count) =  a
        //messed one
        count +=1
        labels(count) = 1
        var lb = new ListBuffer[Int]
        var set = new HashSet[Int]()

        i = 1

        for( t<- 1 until a.length) {
          set += Random.nextInt(MAX_VALUE)
        }

        lb ++= set.toList

        source(count) = lb.toArray
        count +=1

      }
    }


    this.vars = new Array[Variable](MAX_VALUE)
    for(i <-0 until MAX_VALUE) {
      vars(i) = new CategoricalVariable(i, Array(0,1))
    }

    val nbrFeatures = vars.size
    //train, test
    val rowSize = MAX_VALUE

    this.trainData = new MappedStore(rowSize,  source.toArray, labels.toArray, 2, nbrFeatures)

    val af = new ParallelForestBuilder( nbrWorkers  ) with Specs
    af.growForest()


    println("good luck!")

  }

  def scan(  tree:RandomTree ) {
    val src = Source.fromInputStream(new FileInputStream(path))
    val lines = src.getLines()

    var stats = new HashMap[Int,ArrayBuffer[String]]

    while(lines.hasNext  ) {
      val line = lines.next()
      val separator = line.indexOf(",")
      if(separator >0 ){
        val cat = line.substring(0, line.indexOf(","))
        val catId = cats.map.get(cat)
        val data = new ArrayBuffer[Int]
        val tokens = line.substring(separator).split(",").toSet
        for( t<- tokens; if t.length>0) {
          tokenMap.map.get(t) match {
            case Some(v) =>
              data.+=:(v % MAX_VALUE)
            case None =>
              throw new Exception
          }
//          val v = tokenMap.map.get(t).get   % MAX_VALUE
//          data.+=:(v)
        }
        //Build sample
        val bits = new util.BitSet()
        for(v<-data){
          bits.set(v)
        }
        val sample = new BitSample(bits, MAX_VALUE)
        val leaf = tree.dropDown(sample)

        if( leaf.stats(0) > 2 && leaf.stats(1)==0) {
          //println(leaf.id + "->" + cat)
          stats.get(leaf.id) match {
            case Some(v) =>
              v += (cat)
              stats += leaf.id -> v
            case None =>
             val buff:ArrayBuffer[String] = new ArrayBuffer[String]()
             buff += cat
              stats += leaf.id -> buff
          }
        }
      }

    }
    for(s <- stats){
      if(s._2.size>1  ) {
        println(s._1 + "->" +s._2)
      }
    }
    //println(stats)
    src.close()
  }

  def firstPass() =  {
    val src = Source.fromInputStream(new FileInputStream(path))
    val lines = src.getLines()
    val cats =   new TokenMap
    val tokenMap = new TokenMap
    var lineCount =0


    while(lines.hasNext  ) {
      val line = lines.next()
      val separator = line.indexOf(",")
      if(separator >0 ){
        val cat = line.substring(0, line.indexOf(","))
        cats + cat

        val tokens = line.substring(separator).split(",").toSet
        for( t<- tokens) {
          tokenMap + t
        }
        lineCount +=1
      }

    }
    src.close()
    (cats, lineCount, tokenMap)
  }

  class MappedStore(rowSize:Int, source: Array[Array[Int]], val labels: Array[Int],
    val nbrLabels:Int,   val numFeatures:Int) extends DataSource{
    
    val bitSets = new Array[util.BitSet](source.length)
    var i = 0
    while( i < source.length ){
      bitSets(i) = new util.BitSet()
      for(v <- source(i)) {
        bitSets(i).set(v)  
      }
      i  += 1
    }

    override def getSampleAt(rowNbr: Int):  Sample =  {
      new BitSample(bitSets(rowNbr), rowSize)
    }

    override val nbrSamples:Int = labels.length

    override def getValue(rowNbr: Int, col: Int) =
      if(bitSets(rowNbr).get(col)) 1 else 0


    override def getLabel(row: Int) = labels(row)
  }

  trait Specs extends ForestSpecifics
    with SamplerProvider
    with DataSourceProvider
    with OutOfBagTestStrategy
    with PersistentTreeFinalizer {

    val varSelector = new FixedSampleSelector(nbrTry)
    val trainData = ProductCategories.trainData
    val testData = null
    val variables  = ProductCategories.vars .toList
    val forestSize = ProductCategories.forestSize
    val sampleStrategy = BootstrapSampleStrategy
    val outputPath = ProductCategories.outputPath
  }
}

class BitSample (val bitSet:util.BitSet, length:Int) extends Sample{

  override def apply(v:Int) = if (bitSet.get(v) ) 1 else 0

  override def empty ()  {  bitSet.clear()  }

  val size =  length
}


class TokenMap {
  var idx = 0

  var map = new HashMap[String,Int]
  def size = map.size

  def messedUp () :TokenMap  = {
    val result = new TokenMap
    val rnd = new Random
    for(k <- map.keySet) {
      result.map += (k -> rnd.nextInt(idx))
    }

    result
  }

  def value (str:String) = map.getOrElse(str, throw new IllegalStateException(s"invalid input [$str]"))

  def + (str:String) {
    if(str.length>0 && !map.contains(str)){
      map += str -> idx
      idx +=1
    }
  }
}