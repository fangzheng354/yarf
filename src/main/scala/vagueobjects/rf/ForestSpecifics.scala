package vagueobjects.rf

import vagueobjects.rf.variables.{VariableSelector, Variable}
import vagueobjects.rf.construction.Sampler

trait ForestSpecifics {
  val varSelector:VariableSelector
  val variables:List[Variable]
  val forestSize:Int
  val sampleStrategy:Sampler
  val outputPath:String
}
