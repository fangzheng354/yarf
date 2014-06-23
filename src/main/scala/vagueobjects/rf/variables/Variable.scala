package vagueobjects.rf.variables

import scala.util.Random

@SerialVersionUID(3419553940003022221L)
trait Variable extends Serializable{
  def id:Int
}


case class OrdinalVariable(varPosition:Int) extends Variable   {
  override val id = varPosition
}


trait Categorical extends Variable {
  def queries:Array[Query]
}



case class CategoricalVariable(varPosition:Int, values :Array[Int])  extends Categorical{
  val qs = new Array[Query] (values.length)
  for(q <-0 until values.length){
    qs(q) = new CategoricalQuery(varPosition, values(q))
  }

  override val id = varPosition

  def queries = qs

}

case class CategoricalMultiValueVariable(varPosition:Int, values :Array[Int], selection:Int)  extends Categorical{
  val qs = new Array[Query] (values.length)
  for(q <-0 until values.length){
    qs(q) = new CategoricalQuery(varPosition, values(q))
  }

  override val id = varPosition

  def queries = {
    Random.shuffle(qs.toList).slice(0,selection).toArray
  }

}