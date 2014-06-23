package vagueobjects.rf.train

import scala.collection.mutable.ListBuffer

object RFMessage{

  /**
   * Initiates random forest build
   */
  case object GrowForest

  /**
   * Instructs the worker to build {@code nbrTrees}
   */
  case class GrowTrees(nbrTrees:Int)

  case class PartialResult (partial:Array[ListBuffer[ Int ]])

  /**
   * Forest is ready
   */
  case object BuildComplete
}
