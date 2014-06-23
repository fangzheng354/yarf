package vagueobjects.rf.construction

object NodeAttributes {
  val NONE = -1
  //Attributes to fill data array
  val NODE_ID = 0
  val IS_LEAF = 1
  val VARIABLE = 2
  val VALUE = 3
  val DATA_START = 4
  val DATA_END = 5
  val PARENT = 6
  //for leafs  - stats are stored from this position
  val LEAF_STATS_START = 7
  //for nodes with children (always 2)
  val LEFT_CHILD_ID = 7
  val RIGHT_CHILD_ID = 8
}
