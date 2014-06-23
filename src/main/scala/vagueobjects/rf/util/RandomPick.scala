package vagueobjects.rf.util

import scala.util.Random

//http://eyalsch.wordpress.com/2010/04/01/random-sample/
object RandomPick {
  /**
   * Randomly select {@code numValues} members from a given array.
   * @param array   array of random selections
   * @param numValues
   */
  def randomSelection(array:Array[Int], numValues:Int):Array[Int] = {
    require(numValues < array.length)
    ???
  }
}
