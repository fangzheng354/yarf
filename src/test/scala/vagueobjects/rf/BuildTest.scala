package vagueobjects.rf

import org.scalatest.FunSpec
import vagueobjects.rf.construction._
import vagueobjects.rf.variables._
import vagueobjects.rf.variables.CategoricalVariable
import scala.util.Random

class BuildTest extends FunSpec{
  val selector = SimpleSelector

  describe("Two variables, only one relevant"){
    it("expect single split"){
      val source = new InMemoryStore
      //fill it
      Seq(
        //only the variable is relevant
        (Array(0, 0), 0),  (Array(0, 1), 0),
        (Array(1, 0), 1),  (Array(1, 1), 1)
      ) .foreach {  f => source append (f._1, f._2) }

      source.commit()
      val v1 = new CategoricalVariable(0, Array(0, 1))
      val v2 = new CategoricalVariable(1, Array(0, 1))
      implicit val variables = List(v1, v2)
      val sampler = new DefaultSampler (4)
      val builder = new TreeBuilder(source, variables, selector, sampler)
      val tree = builder.build()
      assert(builder.numNodes ==3)
      assert(tree.numLeaves == 2)


    }

  }

  describe("2 variables, both  important"){
    it("produce 2 splits") {
      val source = new InMemoryStore
      //fill it
      Seq(
        (Array(0, 0), 0),(Array(0, 0), 0),  (Array(0, 1), 1),
        (Array(1, 0), 1),  (Array(1, 1), 1)
      ) .foreach {  f => source append (f._1, f._2) }

      source.commit()

      val v1 = new CategoricalVariable(0, Array(0, 1))
      val v2 = new CategoricalVariable(1, Array(0, 1))
      val variables = List(v1, v2)
      val sampler = new DefaultSampler (5)
      val builder = new TreeBuilder(source, variables, selector, sampler)
      val tree = builder.build()
      assert(builder.numNodes ==5)
      assert(tree.numLeaves == 3)
    }
  }


  describe("Case of 2 classes"){
    it("produces appropriate statistics"){
      val source = new InMemoryStore
      //fill it
      Seq(
        (Array(0,0), 0),(Array(0, 1), 0),
        (Array(1, 0), 1),(Array(1,1), 1),(Array(1, 1), 0)
      ) .foreach {  f => source append (f._1, f._2) }
      source.commit()
      val variables = List(new CategoricalVariable(0, Array(0,1)), new CategoricalVariable(1, Array(0,1)))
      val sampler = new DefaultSampler (5)
      val builder = new TreeBuilder(source, variables, selector, sampler)
      val tree = builder.build()
      val leafItr = new LeafIterator(tree)
      var stats = leafItr.next().stats
      assert(java.util.Arrays.equals(stats,Array(2,0)))

      stats =leafItr.next().stats
      assert(java.util.Arrays.equals(stats,Array(1,1)))

      stats =leafItr.next().stats
      assert(java.util.Arrays.equals(stats,Array(0,1)))

      assert(!leafItr.hasNext)
    }

    it("Same result with re-shuffled arrays"){
       val source = new InMemoryStore
      //fill it
      Seq(
        (Array(1,0), 1),(Array(0, 0), 0),
        (Array(1,1), 0),(Array(1,1), 1),(Array(0, 1), 0)
      ) .foreach {  f => source append (f._1, f._2) }
      source.commit()
      val variables = List(new CategoricalVariable(0, Array(0,1)), new CategoricalVariable(1, Array(0,1)))
      val sampler = new DefaultSampler (5)
      val builder = new TreeBuilder(source, variables, selector, sampler)
      val tree = builder.build()
      val leafItr = new LeafIterator(tree)
      var stats = leafItr.next().stats
      assert(java.util.Arrays.equals(stats,Array(2,0)))

      stats =leafItr.next().stats
      assert(java.util.Arrays.equals(stats,Array(1,1)))

      stats =leafItr.next().stats
      assert(java.util.Arrays.equals(stats,Array(0,1)))

      assert(!leafItr.hasNext)
    }
  }

  describe("3 classes, 3 variables"){
    it("Produces 2 leafs"){
      val source = new InMemoryStore
      //fill it
      Seq(
        (Array(0,1,0), 0),(Array(0, 0, 1), 0),(Array(0,  1, 0), 0),
        (Array(0,  1, 0), 0), (Array(1,  1, 0), 1)
      ) .foreach {  f => source append (f._1, f._2) }
      source.commit()
      val variables = List(new CategoricalVariable(0, Array(0,1)),
        new CategoricalVariable(1, Array(0,1)), new CategoricalVariable(1, Array(0,1)))
      val sampler = new DefaultSampler(5)
      val builder = new TreeBuilder(source, variables, selector, sampler)
      val tree = builder.build()
      val leafItr = new LeafIterator(tree)
      val firstLeaf = leafItr.next()

      var stats = firstLeaf.stats
      assert(java.util.Arrays.equals(stats,Array(0,1)))

      stats = leafItr.next().stats
      assert(java.util.Arrays.equals(stats,Array(4, 0)))

      assert(!leafItr.hasNext)

    }
  }
  describe("A super random tree") {
    it("allows any path to run through"){
      val source = new InMemoryStore
      val rnd = new Random
      val size = 30
      //fill it
      val numRows = 100
      Range(0, numRows).map(i=>(randomRow(size, rnd), i % size))
        .foreach {  f => source append (f._1, f._2) }
      source.commit()
      assert(source.nbrSamples==numRows)
      val variables = Range(0, 10).map(i=>new OrdinalVariable(i)).toList

      val forestSize = 100
      val forest = new Array[RandomTree](forestSize)
      val sampler = new DefaultSampler (source.nbrSamples)
      for(t <- 0 until forestSize){
        val builder = new TreeBuilder(source, variables, selector, sampler)
        forest(t) = builder.build()
      }
      for(tree <- forest){
        for(i <- 0 until 20){
          val path =  randomRow(size, rnd)
          val leaf = tree.dropDown(new ArraySample(path))
          var countZero = 0
          leaf.stats.foreach{i => if(i == 0) countZero +=1}
          assert(countZero == size -1)
        }
      }

    }
  }

  def randomRow(size:Int, rnd:Random ) = Array.ofDim[Int](size).map( _ => rnd.nextInt(10))


}

