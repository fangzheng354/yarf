package vagueobjects.rf.construction

import java.util

/**
 * A sequence of element representing a layer of nodes used to build the random tree
 */
class  Elements(private var capacity:Int = 1000)  {
  //temporary attributes per slot
  private val ID = 0
  private val PARENT_ID = 1
  private val IS_LEFT_CHILD = 2
  private val IDX_START = 3
  private val IDX_END = 4
  private val IS_LEAF = 5
  private val VAR_ID = 6
  private val VAL_ID = 7
  private val SIZE = 8

  capacity = Math.max(capacity, SIZE)
  var allocatedSize = 0
  var data = new Array[Int](capacity)

  def this(idxStart:Int, idxEnd:Int) {
    this()
    //allocate root 
    Array(
      PARENT_ID, IS_LEFT_CHILD, IS_LEAF, VAR_ID, VAL_ID).foreach(data(_) = NodeAttributes.NONE)
    this.data(ID) = 0
    this.data (IDX_START) = idxStart
    this.data (IDX_END) = idxEnd
    this.allocatedSize =SIZE
  }

  def getIdxStart (offset:Int) =  data(offset + IDX_START)
  def getIdxEnd (offset:Int) =  data(offset + IDX_END)
  def getId(offset:Int) = data(offset + ID)
  def setLeaf(offset:Int, flag:Boolean=true) = data(offset +IS_LEAF)  = if(flag) 1 else 0
  def setVarId(offset:Int, value:Int) = data(offset+VAR_ID) = value
  def setValId(offset:Int, value:Int) = data(offset+VAL_ID) = value

  def foreach(f:Element => Unit) {
    val itr = new Itr
    while(itr.hasNext){
      f(itr.next())
    }
  }

  def size() = allocatedSize

  def isEmpty() = allocatedSize==0

  def addLeftChild(id:Int,parentId:Int, idxStart:Int, idxEnd:Int) {
    add(id, isLeftChild = true, parentId, idxStart, idxEnd)
  }
  def addRightChild(id:Int,parentId:Int, idxStart:Int, idxEnd:Int) {
    add(id, isLeftChild = false, parentId, idxStart, idxEnd)
  }



  private def add (id:Int, isLeftChild:Boolean, parentId:Int, idxStart:Int, idxEnd:Int){
    ensureCapacity()
    data(allocatedSize + ID) = id
    data(allocatedSize + PARENT_ID) = parentId
    data(allocatedSize + IS_LEFT_CHILD) = if(isLeftChild) 1 else 0
    data(allocatedSize + IDX_START) = idxStart
    data(allocatedSize + IDX_END) = idxEnd
    data(allocatedSize + IS_LEAF) = 1
    data(allocatedSize + VAR_ID) = -1
    data(allocatedSize + VAL_ID) = -1
    allocatedSize += SIZE
  }

  private def ensureCapacity (){
    if(allocatedSize + SIZE >= capacity){
      val newCap = capacity*3/2  +1
      this.data  = util.Arrays.copyOf(data, newCap)
      capacity = newCap
    }
  }


  class Itr extends Iterator[Element] {
    var spare = new Element(-1,false,false,-1,-1,-1,-1,-1)
    var offset =0

    def hasNext: Boolean = offset < allocatedSize

    def next(): Element = {
      spare.id = data(offset+ID)
      spare.parentId =  data(offset + PARENT_ID)
      spare.idxStart =  data(offset + IDX_START)
      spare.idxEnd =  data(offset + IDX_END)
      spare.isLeftChild =  data(offset + IS_LEFT_CHILD)>0
      spare.isLeaf = data(offset + IS_LEAF)>0
      spare.varId = data(offset + VAR_ID)
      spare.valId = data(offset + VAL_ID)
      offset += SIZE
      spare
    }
  }
}
object Elements {
  def empty = new Elements()
  val ELEMENT_SIZE = 8
}

/** Element of a tree */
case class Element (var id:Int, var isLeftChild:Boolean, var isLeaf:Boolean,
   var parentId:Int, var idxStart:Int, var idxEnd:Int, var valId:Int, var varId:Int)

