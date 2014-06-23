package vagueobjects.rf.construction

import scala.collection.mutable


trait DataSource {

  def getSampleAt(row:Int ) : Sample

  def getValue(row:Int, col:Int) :Int

  def getLabel(row:Int)  :Int

  def nbrSamples:Int

  def nbrLabels:Int


  /**
   * Computes label stats
   */
  def processLeaf(idxStart:Int, idxEnd:Int, index:Array[Int]) ={
    val result = new Array[Int](nbrLabels)
    for(i<-idxStart to idxEnd ){
      val row = index(i)
      val label = getLabel(row)
      result(label)+=1
    }
    result
  }
}

class ArraySample(val arr:Array[Int]) extends Sample{
  override def apply(v:Int) = arr(v)

  override def empty ()  {
    arr map (_=> 0)
  }

  val size = arr.length
}

trait  Sample {
  def apply(v:Int):Int
  def empty()
  val size :Int
}