package vagueobjects.rf.construction

import bak.pcj.map.{IntKeyDoubleChainedHashMap, IntKeyIntChainedHashMap}
import vagueobjects.rf.variables._
import vagueobjects.rf.RandomTree
import java.util.concurrent.atomic.AtomicInteger
import java.util.Arrays._
import bak.pcj.list.IntArrayList
import bak.pcj.set.IntChainedHashSet
import scala.collection.immutable.HashMap


/**
 * Grows a single tree
 * @param dataSource  source of data
 * @param variables list of variables
 *
 */
class TreeBuilder(dataSource: DataSource,
  variables: List[Variable], varSelector:VariableSelector, sampler:Sampler ) {

  val DEFAULT_CAP = 100
  var capacity = DEFAULT_CAP
  var index: Array[Int] = _
  var unused: Array[Int] = _
  //holds tree data as integer array, partitioned into slots
  private var data = new Array[Int](capacity)
  private var position = 0
  private val offsetIndex = new IntKeyIntChainedHashMap()
  //used to compute variable importance
  private val variableImportance = new IntKeyDoubleChainedHashMap
  private val nbrLabels = dataSource.nbrLabels
  private val leafs = new IntArrayList
  private val idGenerator = new AtomicInteger(0)

  import NodeAttributes._

  def reset(){
    this.capacity = DEFAULT_CAP
    this.data = new Array[Int](capacity)
    this.position = 0
    this.offsetIndex.clear()
    this.variableImportance.clear()
    this.leafs.clear()
    this.idGenerator.set(0)
  }

  /**
   * Builds a new instance of random tree
   *
   * @return random tree
   */
  def build() = {
    makeSamples()

    //Root partition, takes the entire table
    var currentLayer: Elements = new Elements(0, dataSource.nbrSamples - 1)
    var nextLayer: Elements = Elements.empty
    //In this loop, each element of the layer adds children to the next layer
    //This stops when all elements in current layer are leaves
    while (!currentLayer.isEmpty) {
      var pos =0
      while (pos <  currentLayer.size ) {
        val interval = new Interval(currentLayer.getIdxStart(pos), currentLayer.getIdxEnd(pos))
        val labelStats = computeLabelStats(interval)
        fillNextLayer(pos, interval, currentLayer, nextLayer, labelStats)
        pos += Elements.ELEMENT_SIZE
      }
      //store current layer
      currentLayer.foreach(save)
      currentLayer = nextLayer
      nextLayer = new Elements()
    }
    trimToSize()

    val newOffsets = new IntKeyIntChainedHashMap()
    val keyItr = offsetIndex.keySet().iterator()
    while(keyItr.hasNext){
      val k = keyItr.next()
      newOffsets.put(k, offsetIndex.get(k))
    }
    newOffsets.trimToSize()

    new RandomTree(data.clone(), leafs.toArray.clone(), unused.clone(), nbrLabels,
      newOffsets, variables)
  }


  def makeSamples() {
    val s =  sampler.sample( )
    index = s._1.clone()
    unused = s._2
  }


  def numNodes = offsetIndex.size()

  def numLeafs = leafs.size()

  def fillNextLayer(pos: Int, interval:Interval, currentLayer: Elements,
                    nextLayer: Elements, labelStats:Array[Int]) {

    if (interval.idxStart >= interval.idxEnd) {
      currentLayer.setLeaf(pos)
      return
    }

    val split = splitCandidate(pos, currentLayer, interval, labelStats)
    if(split == null) {
      currentLayer.setLeaf(pos)
    }  else {
      var partition = interval.idxStart - 1
      var p = interval.idxStart
      while (p <=interval.idxEnd) {
        //find actual data at pos
        val i = index(p)
        val sample = dataSource.getSampleAt(i)
        //do swapping similar to how QuickSort does it
        if (split.isTrue(sample)) {
          partition += 1
          val temp = index(p)
          index(p) = index(partition)
          index(partition) = temp
        }
        p+=1
      }
      updateImportanceFactor(split.query.id, split.purityGain)
      currentLayer.setVarId(pos, split.query.id)
      currentLayer.setValId(pos, split.query.value)
      currentLayer.setLeaf(pos, false)
      val leftChildId = idGenerator.incrementAndGet()
      val id = currentLayer.getId(pos)
      nextLayer.addLeftChild(leftChildId, id, interval.idxStart, partition)
      val rightChildId = idGenerator.incrementAndGet()
      nextLayer.addRightChild(rightChildId, id, partition + 1, interval.idxEnd)
    }


  }


  private def updateImportanceFactor(id: Int, purityGain: Double) {
    val increment = variableImportance.get(id) + purityGain
    variableImportance.put(id, increment)
  }




  def splitCandidate(pos: Int, src: Elements, interval: Interval, labelStats:Array[Int]) = {
    //we can run this in parallel
    val splitVariables =  varSelector.choose(variables)

    var split:  Split  = null

    for (variable <- splitVariables) {
      val candidate =   bestSplit(variable, interval, labelStats)
      if(split ==null || candidate!=null && candidate.betterThan(split)){
        split =   candidate
      }

    }
    split
  }


  private def bestSplit(variable: Variable, interval: Interval, labelStats:Array[Int]):  Split  =
    variable match {
      case ord: OrdinalVariable => bestOrdSplit(ord, interval, labelStats)
      case cat: Categorical  => bestCatSplit(cat, interval)
      case _ => null
    }


  private def bestOrdSplit(variable: OrdinalVariable, interval: Interval, labelStats:Array[Int]):  Split  = {
    val set = new IntChainedHashSet()
    var i = 0
    while (i <= interval.idxEnd) {
      val row = index(i)
      val data = dataSource.getSampleAt(row)
      val value = data(variable.varPosition)
      set.add(value)
      i += 1
    }
    val splits = set.toArray()
    if (splits.length == 0) {
      null
    } else {

      sort(splits)

      val queries:  Array[Query] = Array.tabulate(splits.length) {
        i => new OrdinalQuery(variable.varPosition, splits(i))
      }
      val splitters: Array[Splitter] =  Array.tabulate(splits.length) {
        i => new Splitter(nbrLabels, queries(i))
      }
      //val labelStats = computeLabelStats(interval)
      val labelStats = new Array[Int](dataSource.nbrLabels)
      var row =   interval.idxStart
      while (row <= interval.idxEnd) {
        val sample = index(row)
        val label = dataSource.getLabel(sample)
        labelStats(label) += 1

        val data = dataSource.getSampleAt(sample)
        for (spl <- splitters) {      
          spl.process(data, label)
        }
        row +=1
      }
      makeSplit(interval, labelStats , splitters, queries)
    }

  }


  private def bestCatSplit(variable: Categorical, interval: Interval): Split = {
    val queries = variable.queries
    val splitters =queries.map(q => new Splitter(nbrLabels, q))
    val labelStats = computeLabelStats(interval)
    val size = interval.idxEnd - interval.idxStart +1
    val s =  new Splitters(splitters, labelStats, size)
    var row =  interval.idxStart
    while (row <=interval.idxEnd) {
      val idx = index(row)
      val label = dataSource.getLabel(idx)
      val value = dataSource.getValue(idx, variable.id)
      s.process( value, label)
      row +=1
    }

    makeSplit(interval, labelStats , splitters,  queries)
  }

  private def makeSplit(interval: Interval, stats:  Array[Int],
                        splitters: Array[Splitter], queries: Array[Query]):  Split = {

    var originalImpurity = 1d
    var lbl = 0
    while (lbl <  stats.length) {
      val prob = stats(lbl) / (interval.idxEnd - interval.idxStart + 1d)
      originalImpurity -= prob * prob
      lbl += 1
    }
    if (originalImpurity != 0) {
      var bottomImpurity = Double.MaxValue
      var bestSplitIdx = -1
      var q = 0
      while (q <  splitters.length) {
        val splitter = splitters(q)
        if (splitter.getPosCount > 0 && splitter.getNegCount > 0) {
          var posImp = 1d
          var l =0
          while (l <  stats.length) {
            val posProb = splitter.getPosStats(l) / splitter.getPosCount
            posImp -= posProb * posProb
            l +=1
          }
          var negImp = 1d
          l=0
          while (l < stats.length) {
            val negProb = splitter.getNegStats(l) / splitter.getNegCount
            negImp -= negProb * negProb
            l +=1
          }
          val tau = splitter.getPosCount / (splitter.getPosCount + splitter.getNegCount)
          val imp = tau * posImp + (1d - tau) * negImp
          if (imp == 0d || imp <= bottomImpurity) {
            bestSplitIdx = q
            bottomImpurity = imp
          }
        }
        q+=1
      }
      val delta = originalImpurity - bottomImpurity
      //split is worthless unless there is some gain in purity
      if (delta > 0) {
        return  new Split(queries(bestSplitIdx), delta )
      }
    }
    null
  }


  private def computeLabelStats  (interval: Interval ) = {
    val result = new Array[Int](dataSource.nbrLabels)
    var row =  interval.idxStart
    while (row <= interval.idxEnd) {
      val idx = index(row)
      val label = dataSource.getLabel(idx)
      result(label) += 1
      row+=1
    }
    result
  }

 

  /**
   * Copies data from current element to array of tree nodes
   * @param element
   */
  def save(element: Element) {
    ensureCapacity()
    offsetIndex.put(element.id, position)
    add(NODE_ID, element.id)
    add(IS_LEAF, if (element.isLeaf) 1 else 0)
    add(VARIABLE, element.varId)
    add(VALUE, element.valId)
    add(DATA_START, element.idxStart)
    add(DATA_END, element.idxEnd)
    add(PARENT, element.parentId)
    if (element.id > NONE) {
      //not a root, so it has a parent
      val parentPos = offsetIndex.get(element.parentId)
      if (element.isLeftChild) {
        data(parentPos + LEFT_CHILD_ID) = element.id
      } else {
        data(parentPos + RIGHT_CHILD_ID) = element.id
      }
    }
    position += LEAF_STATS_START
    if (element.isLeaf) {
      leafs.add(element.id)
      val stats = dataSource.processLeaf(element.idxStart, element.idxEnd, index)
      for (i <- 0 until stats.length) {
        ensureCapacity()
        data(position + i) = stats(i)
      }
      position += stats.length
    } else {
      //just make room for 2 children
      //this space will be filled by them later
      position += 2
    }
  }

  private def trimToSize() {
    data = copyOf(data, position)
    offsetIndex.trimToSize()
    leafs.trimToSize()
  }

  private def add(fld: Int, v: Int) {
    data(position + fld) = v
  }

  private def ensureCapacity() {
    if (position + nbrLabels + 20 >= data.length) {
      val newCap = (position + nbrLabels) * 3 / 2 + 1
      data = copyOf(data, newCap)
      capacity = newCap
    }
  }

}

case class Interval(idxStart: Int, idxEnd: Int)