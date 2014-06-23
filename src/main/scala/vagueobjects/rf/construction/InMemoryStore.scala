package vagueobjects.rf.construction

import java.util.Arrays._
import bak.pcj.set.IntChainedHashSet

 
class InMemoryStore (var capacity:Int = 1000) extends DataSource{

  private var nbrRows  = 0
  private var data  = new Array[Array[Int]](capacity)
  private val seenLabels = new IntChainedHashSet()
  private var labelData = new Array[Int](capacity)

  def getSampleAt(row:Int ) = new  ArraySample(data(row))
  def getLabel(row:Int) = labelData(row)
  def nbrSamples = nbrRows
  var nbrLabels:Int = _
  var rowLength:Int = _


  def append (row:Array[Int], label:Int) = {
    if(rowLength==0){
      rowLength = row. length
    }
    require(rowLength == row. length)
    seenLabels.add(label)

    ensureCapacity(nbrRows)
    this.data(nbrRows) = row
    this.labelData(nbrRows) = label
    nbrRows+=1
    this
  }

  /**
   * Called when we are done appending
   * @return
   */
  def commit() ={
    this.nbrLabels = seenLabels.size()
    seenLabels.clear()

    this.labelData = copyOf(this.labelData, nbrRows)
    val _data  = new Array[Array[Int]](nbrRows)
    for(i <-0 until nbrRows){
      _data(i)   = data(i)
    }
    this.data =  _data
    this
  }



  private def ensureCapacity(pos:Int){
    if(pos>=labelData.length){
      val oldCap = this.labelData.length
      val newCap =  oldCap*3/2  +1
      this.labelData = copyOf(this.labelData, newCap)
      val data  = new Array[Array[Int]](newCap)
      for(i <-0 until newCap){
        data(i) = if(i<oldCap)
          this.data (i)
        else
          new Array[Int](rowLength)
      }
      this.data = data
      this.capacity = newCap
    }
  }

  override def getValue(row: Int, col: Int): Int = data(row)(col)
}
