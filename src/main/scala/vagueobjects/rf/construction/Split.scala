package vagueobjects.rf.construction

import vagueobjects.rf.variables.Query

class Split(val query:Query, val purityGain:Double) {

  def betterThan(other:Split) = purityGain > other.purityGain

  def isTrue(data:Sample) = query.isPositiveOn(data)
}


class Splitter(nbrLabels:Int, val query:Query) {
  var posCount = 0d
  var negCount = 0d
  var posStats = new Array[Double](nbrLabels)
  var negStats = new Array[Double](nbrLabels)

  def process (path: Sample, label:Int) {
    if(query.isPositiveOn(path)){
      posCount+=1d
      posStats(label) += 1d
    } else {
      negCount+=1d
      negStats(label) += 1d
    }
  }

  def getPosCount = posCount

  def getNegCount = negCount

  def getPosStats = posStats

  def getNegStats = negStats

  def canSplit = posCount>0 && negCount>0
}

class Splitters(splitters: Array[Splitter], labelStats:Array[Int], numRows:Int) {
   var i=0
   while(i<splitters.length){
     splitters(i).negCount = numRows
     var l=0
     while(l < labelStats.length){
       splitters(i).negStats(l) = labelStats(l)
       l += 1
     }
     i +=1
   }

  def process(  value:Int,  label:Int){
      val spl =  splitters(value)
      spl.posCount +=1
      spl.posStats(label) +=1
      spl.negCount -=1
      spl.negStats(label) -=1
  }

}