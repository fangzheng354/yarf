package vagueobjects.rf.construction

/**
 * DataStore backed by 2D array
 */
class KVStore(source: Array[Array[Int]], val labels: Array[Int],
              val nbrLabels:Int,   val numFeatures:Int) extends DataSource{
  override def getSampleAt(rowNbr: Int):  Sample =   new ArraySample(source(rowNbr))
  override val nbrSamples:Int = labels.length
  override def getValue(rowNbr: Int, col: Int) =  source(rowNbr)(col)
  override def getLabel(row: Int) = labels(row)
}