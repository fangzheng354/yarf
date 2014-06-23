package vagueobjects.rf


import org.scalatest.FunSpec
import vagueobjects.rf.construction._
import vagueobjects.rf.variables._
import vagueobjects.rf.construction.Element
import vagueobjects.rf.variables.CategoricalVariable

class SimpleBuildTest extends FunSpec{
  val selector = SimpleSelector


  describe("Simple element tests"){
    it("Slots should not run out of space"){
      //small element
      val slots =new Elements(1)
      (0 until 10000).foreach(_=>slots.addLeftChild(0,  0, 100, 200))
    }

    val variable = new CategoricalVariable(0, Array(0, 1))
    val variables = List(variable)
    val source = DummyTable


    it("Storing a single slots adds data to a tree builder correctly"){
      val builder = new TreeBuilder(source, variables, selector, new DefaultSampler(1))
      val slot = new Element(0, isLeftChild = true,isLeaf =  true, -1, 0, 100, 1, 1)
      builder.save(slot)
      assert(builder.numLeafs==1)
      assert(builder.numNodes==1)
    }
    it("Storing 2 slots adds data to a tree builder correctly"){
      val builder = new TreeBuilder(source, variables, selector, new DefaultSampler(1))
      val slot1 = new Element(0, isLeftChild = true,isLeaf =  true, -1, 0, 100, 1, 1)
      builder.save(slot1)
      val slot2 = new Element(1, isLeftChild = true,isLeaf =  false, -1, 0, 100, 1, 1)
      builder.save(slot2)

      assert(builder.numLeafs==1)
      assert(builder.numNodes==2)
    }
  }



  describe("One-node tree"){
    it("just builds") {
      val variable = new CategoricalVariable(0, Array(0, 1))
      val variables = List(variable)
      val source = new InMemoryStore  append (Array(1), 0) commit

      val builder = new TreeBuilder(source, variables, selector,  new DefaultSampler(1))
      val tree = builder.build()
      assert(tree.numLeaves == 1)
      assert(builder.numNodes == 1)
    }
  }



  describe("2-leaf tree") {
    it("Has one node and 2 leafs"){
      val source = new InMemoryStore
      //fill it
      Seq(
        //value 1 produces 1 mostly
        (Array(1), 0),  (Array(1), 1), (Array(1), 0),
        (Array(1), 1),  (Array(1), 1), (Array(1), 1),
        (Array(1), 1),  (Array(1), 0), (Array(1), 1),
        //value 0 produces 0 mostly
        (Array(0), 0),  (Array(0), 0), (Array(0), 0)
      ) .foreach {  f => source append (f._1, f._2) }

      source.commit()
      val variable = new CategoricalVariable(0, Array(0, 1))
      val variables = List(variable)

      val builder = new TreeBuilder(source, variables, selector,  new DefaultSampler(12))
      val tree = builder.build()
      assert(builder.numNodes ==3)
      assert(tree.numLeaves == 2)
    }
  }

  describe("Simple test with ordinals"){
    it("produces the same tree"){
      val source = new InMemoryStore
      Seq(
        (Array(1), 0),(Array(1), 0),
        (Array(1), 1),(Array(1), 1),(Array(1), 1),(Array(1), 1),(Array(1), 1),
        (Array(0), 0),  (Array(0), 0), (Array(0), 0)
      ) .foreach {  f => source append (f._1, f._2) }
      source.commit()
      implicit val variables = List(new OrdinalVariable(0))
      val builder = new TreeBuilder(source, variables, selector,  new DefaultSampler(10))
      val tree = builder.build()
      assert(builder.numNodes == 3)
      assert(tree.numLeaves == 2)
    }
    it("similar case "){
      val source = new InMemoryStore
      Seq(
        (Array(0), 0),(Array(1), 0),
        (Array(2), 0),(Array(3), 0),(Array(4), 0),(Array(5), 1),(Array(6), 1),
        (Array(7), 1)
      ) .foreach {  f => source append (f._1, f._2) }
      source.commit()
      val selector = SimpleSelector
      val variables = List(new OrdinalVariable(0))
      val sampler = new DefaultSampler(8)
      val builder = new TreeBuilder(source, variables, selector, sampler)
      val tree = builder.build()
      assert(builder.numNodes == 3)
      assert(tree.numLeaves == 2)
    }
  }

  describe("More advanced test with ordinals"){

    it("Ordinal variable with lots of variations, but only one split!"){
      implicit val source = new InMemoryStore
      Seq(
        (Array(0), 0),(Array(1), 0),
        (Array(2), 0),(Array(3), 0),(Array(4), 0),
        (Array(5), 1),(Array(6), 1), (Array(7), 1)
      ) .foreach {  f => source append (f._1, f._2) }
      source.commit()
      val variables = List(new OrdinalVariable(0))
      val builder = new TreeBuilder (source, variables, selector,  new DefaultSampler(8))
      val tree = builder.build()
      assert(builder.numNodes == 3)
      assert(tree.numLeaves == 2)
    }
  }

  describe("Small tree"){
    it("Must not split"){
      val source = new InMemoryStore
      Seq(
        (Array(1), 0),(Array(1), 1),
        (Array(0), 1),(Array(0), 0)
      ) .foreach {  f => source append (f._1, f._2) }
      source.commit()
      val variables = List(new CategoricalVariable(0, Array(0,1)))

      val builder = new TreeBuilder(source, variables, selector,  new DefaultSampler(4))
      val tree = builder.build()
      assert(builder.numNodes == 1)
      assert(tree.numLeaves == 1)

    }
  }
}

object DummyTable extends InMemoryStore {
  override def append (row:Array[Int], label:Int) = this
  override def commit() = this
  override def processLeaf(idxStart:Int, idxEnd:Int, index:Array[Int]) = Array.empty[Int]
}