package vagueobjects.rf.train

import vagueobjects.rf.{ForestSpecifics, Leaf, RandomTree}
import scala.collection.mutable.ListBuffer
import vagueobjects.rf.construction.DataSourceProvider
import scala.collection.immutable.HashMap
import java.io.{ObjectOutputStream, FileOutputStream}
import java.util.UUID


trait PostProcessing {

  def collectVotes(tree:RandomTree,  votes: Array[ListBuffer[ Int ]] )

  def finish(tree:RandomTree)

  def aggregateVotes(predictions: Array[ListBuffer[Int]]): Float

  def collectLeafVotes(leaf: Leaf) = {
    var max = -1
    var maxVote = -1
    for (i <- 0 until leaf.stats.length) {
      if (leaf.stats(i) > max) {
        maxVote = i
        max = leaf.stats(i)
      }
    }
    maxVote
  }
  
  def electWinner(votes: ListBuffer[ Int ]) = {
    var counts = new HashMap[Int, Int]
    for (vote <- votes) {
      val current = counts.getOrElse(vote , 0) + 1
      counts += vote -> current
    }
    var winner = -1
    var maxVote = -1
    for (c <- counts) {
      if (c._2 > maxVote) {
        maxVote = c._2
        winner = c._1
      }
    }
    winner
  }
}

trait DefaultTreeFinalizer {
  def finish(tree:RandomTree) {}
}

trait PersistentTreeFinalizer {
  this: ForestSpecifics =>

  def finish(tree:RandomTree) {
    //give it random name
    val name = UUID.randomUUID() + "-tree.ser"
    val fos = new FileOutputStream(s"$outputPath/$name" )
    val out  = new ObjectOutputStream(fos)
    try{
      out.writeObject(tree)
    } finally {
      out.close()
    }
  }
}

trait OutOfBagTestStrategy extends PostProcessing {
  this: DataSourceProvider   =>
  
  def collectVotes(tree:RandomTree, votes: Array[ListBuffer[Int]] ){
    for (r <- 0 until trainData.nbrSamples) {
      if (tree.oobSample(r) > 0) {
        val sample = trainData.getSampleAt(r)
        val leaf = tree.dropDown(sample)
        votes(r) +=  collectLeafVotes(leaf)
      }
    }
  }
  def aggregateVotes(predictions: Array[ListBuffer[  Int ]]) = {

    var errs: Int = 0
    for (r <- 0 until trainData.nbrSamples) {
      val expectation = trainData.getLabel(r)
      val vote = electWinner(predictions(r))
      if (vote != expectation) errs += 1
    }

    errs.toFloat / trainData.nbrSamples.toFloat
  }
}


trait SimpleTestStrategy extends PostProcessing {
  this: DataSourceProvider   =>

  def collectVotes(tree:RandomTree, votes: Array[ListBuffer[Int]] ){
    for (r <- 0 until testData.nbrSamples) {
      val path = testData.getSampleAt(r)
      val leaf = tree.dropDown(path)
      votes(r) +=  collectLeafVotes(leaf)
    }
  }

  def aggregateVotes(predictions: Array[ListBuffer[  Int ]]) = {

    var errs: Int = 0
    for (r <- 0 until testData.nbrSamples) {
      val expectation = testData.getLabel(r)
      val vote = electWinner(predictions(r))
      if (vote != expectation) errs += 1
    }

    errs.toFloat / testData.nbrSamples.toFloat
  }
}