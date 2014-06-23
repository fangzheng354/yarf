package vagueobjects.rf.clustering


import com.google.common.hash.{Funnels, BloomFilter}
import scala.collection.immutable.HashMap
import vagueobjects.rf.construction.DataSource
import bak.pcj.list.{IntArrayList, IntList}
import vagueobjects.rf.Tree
import scala.collection.immutable

class ClusterBuilder    {

  this:ProximitySpecs with DataSource with Forest =>

  type IntPair = (Int,Int)

  val filters = List.fill(support)(new Filter )

  private var map =   new immutable.HashMap[IntPair, Int]

  def makeProximities():Proximity = {
    trees foreach processTree
    new Proximity {
      override def proximity(id1: Int, id2: Int): Double = {
        val p = if (id1 < id2) (id1, id2) else (id2, id1)
        val count: Int = map.getOrElse(p, 0)
        count.toDouble / numTrees.toDouble
      }
    }
  }

  def processTree(tree:Tree)   {
    var leafMap = new HashMap[Int, IntList]
    for(s <- 0 until nbrSamples){
      val sample = getSampleAt(s)
      val leafId = tree.dropDown(sample).id
      val value = leafMap.getOrElse(leafId, new IntArrayList())
      value.add(leafId)
      leafMap += leafId -> value
    }
    //Collect all pairs per leaf
    for(leafIds <- leafMap.values) {
      val arr = leafIds.toArray
      for(a <- 0 until arr.length) {
        for(a1 <- a+1 until arr.length){
          add(toLong(a, a1))
        }
      }
    }

  }

  def toLong(v1:Int, v2:Int):Long = {
    val (u1,u2) = if(v1<v2) (v1,v2) else (v2, v1)
    u1.toLong << 32 | u2.toLong
  }

  private def add(value:Long) = {
    if(!upgradeSupport(value)) {
      //must be highly supported
      val id1 = (value >>>32).toInt
      val id2 = value.toInt
      val p = if (id1<id2 ) (id1, id2) else (id2,id1)
      val count:Int = map.getOrElse(p, 0)
      map += p -> (count+1)
    }
  }
 

  /**
   * Drop {@code value} down the filters.
   * @return true iff all filters say that the value already added
   */
    def upgradeSupport(value:Long):Boolean = {
      for(f <- filters){
        if(f.addIfMissing(value))
          return true
      }
      false
    }

    class Filter  {

      val bloomFilter = BloomFilter.create( Funnels.longFunnel(),  expectedSize  )

      /**
       * If the filter seems to contain the value already, passes it to the next filter and returns true.
       * Otherwise adds the value to the filter filter.
       *
       * @return true iff the filter added this value
       */
      def addIfMissing(value:Long):Boolean =
        if(bloomFilter.mightContain(value)){
          //pass down the chain of filters
          false
        } else {
          bloomFilter.put(value)
          true
        }
    }
}

/**
 * Measures how close 2 entries are
 */
trait Proximity {
  def proximity(id1:Int , id2: Int) :Double
}



