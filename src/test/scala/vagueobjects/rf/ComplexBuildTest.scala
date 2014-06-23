package vagueobjects.rf

import org.scalatest.FunSpec
import vagueobjects.rf.construction._
import vagueobjects.rf.variables._
import vagueobjects.rf.variables.CategoricalVariable

class ComplexBuildTest extends FunSpec{

  val selector = SimpleSelector


  describe("Case of 2 classes"){
    it("produces appropriate statistics"){
      val source = new InMemoryStore
      //fill it
      Seq(
        (Array(0,0), 0),(Array(0, 1), 0),
        (Array(1, 0), 1),(Array(1,1), 1),(Array(1, 1), 0)
      ) .foreach {  f => source append (f._1, f._2) }
      source.commit()
      val sampler = new DefaultSampler (5)
      val variables = List(new CategoricalVariable(0, Array(0,1)), new CategoricalVariable(1, Array(0,1)))

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
      val builder = new TreeBuilder (source, variables, selector, sampler)
      val tree = builder.build()
      val leafItr = new LeafIterator(tree)
      val firstLeaf = leafItr.next()

      var stats = firstLeaf.stats
      assert(java.util.Arrays.equals(stats,Array(2,0)))

      stats =leafItr.next().stats
      assert(java.util.Arrays.equals(stats,Array(1,1)))

      stats =leafItr.next().stats
      assert(java.util.Arrays.equals(stats,Array(0,1)))

      assert(!leafItr.hasNext)

    }
  }


}

