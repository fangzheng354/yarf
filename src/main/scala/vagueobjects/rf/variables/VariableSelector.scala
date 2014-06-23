package vagueobjects.rf.variables

import scala.util.Random

trait VariableSelector {
  def choose(vars:List[Variable]) :List[Variable]
}




object RFSelector extends VariableSelector {
  val minNumberOfVariables = 10

  override def choose(vars:List[Variable]) :List[Variable] = {
    val origSize = vars.size
    require(origSize > minNumberOfVariables)
    val newSize = downSize(origSize).toInt

    Random.shuffle(vars).toList.slice(0, newSize)

  }

  def downSize(length:Int) =  Math.sqrt(length)
}



class FixedSampleSelector(size:Int) extends VariableSelector {
  val minNumberOfVariables = 10

  override def choose(vars:List[Variable]) :List[Variable] = {
    Random.shuffle(vars).toList.slice(0, size)
  }

}


object SimpleSelector extends VariableSelector {
  override def choose(vars:List[Variable]) :List[Variable] = vars
}