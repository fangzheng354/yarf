package vagueobjects.rf

import bak.pcj.map.IntKeyIntMap
import bak.pcj.set.{IntChainedHashSet, IntSet}
import vagueobjects.rf.variables.{OrdinalVariable, Variable}
import vagueobjects.rf.construction.{Sample, NodeAttributes}
import java.util.UUID

@SerialVersionUID(3419553961353022221L)
class RandomTree(override val data:Array[Int], val leafs:Array[Int],
    val oobSample:Array[Int], val numLabels:Int, val index:IntKeyIntMap, variables:List[Variable] )
    extends Tree with Serializable {

  import NodeAttributes._

  private lazy val ordIdList:IntSet = makeOrdinalList()

  def numLeaves = leafs.size

  /**
   * drops sample down the tree
   * @param sample - input path. This method attempts to match tree's nodes to the path
   * @return
   */
  override def dropDown(sample: Sample) = {
    require(sample!=null  )
    var offset = index.get(0)
    var variable = variableAt(offset)
    var value = valueAt(offset)
    var isLeftChild = false

    do {
      val currentValue = sample(variable)
      isLeftChild = if(ordIdList.contains(variable)) {
        //if current variable is ordinal
        value > currentValue
      } else {
        value == currentValue
      }
      val childAttr = if(isLeftChild) LEFT_CHILD_ID else RIGHT_CHILD_ID
      val childId = data(offset + childAttr)

      require(index.containsKey(childId), s"Child $childId is not found: $currentValue")

      offset = index.get(childId)

      variable = variableAt(offset)
      value = valueAt(offset)

    } while(!isLeaf(offset))
    val id = data(offset + NODE_ID)
    val parentId = data(offset + PARENT)

    val stats = new Array[Int](numLabels)
    for(i<-0 until stats.length){
      stats(i) = data(offset + LEAF_STATS_START + i)
    }
    new Leaf(id, parentId, isLeftChild, stats)
  }

  private [this] def isLeaf(offset:Int) = data(offset + IS_LEAF) ==1

  private [this] def variableAt(offset:Int) = data(offset + VARIABLE)

  private [this] def valueAt(offset:Int) = data(offset + VALUE)

  private [this] def makeOrdinalList()  = {
    val ordIdList = new IntChainedHashSet()
    for(i <-0 until variables.length){
      if(variables(i).isInstanceOf[OrdinalVariable]){
        ordIdList.add(i)
      }
    }
    ordIdList
  }

}

case class Leaf(id:Int, parentId:Int, isLeftChild:Boolean, stats:Array[Int])

trait Tree {
  val data:Array[Int]
  def dropDown(path: Sample) :Leaf
}