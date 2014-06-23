package vagueobjects.rf

import vagueobjects.rf.construction.NodeAttributes

class LeafIterator(val tree:RandomTree) extends   Iterator[Leaf] {
  import NodeAttributes._
  var current = 0

  def hasNext: Boolean = current < tree.leafs.length

  def next(): Leaf = {
    val leafId = tree.leafs(current)
    val offset = tree.index.get(leafId)
    val parent = tree.data(offset  +  PARENT)
    val parentOffset = tree.index.get(parent)
    val leftChildId = tree.data(parentOffset + LEFT_CHILD_ID)
    val stats = new Array[Int](tree.numLabels)
    val isLeftChild = leftChildId == leafId
    for(i<-0 until stats.length){
      stats(i) = tree.data(offset + LEAF_STATS_START + i)
    }
    current +=1
    new Leaf(leafId, parent, isLeftChild, stats)
  }
}
