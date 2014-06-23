package vagueobjects.rf.variables

import vagueobjects.rf.construction.Sample

trait Query {
  def isPositiveOn(path:Sample)  :Boolean
  val id:Int
  val value:Int

}

@SerialVersionUID(3419553944353022221L)
sealed class CategoricalQuery(val id:Int, val value:Int) extends Query with Serializable{
  override def isPositiveOn(data:Sample) = data(id) == value
}

@SerialVersionUID(3419553944353011221L)
sealed class OrdinalQuery (val id:Int, val value:Int) extends Query with Serializable{
  override def isPositiveOn(data:Sample) = data(id) < value
}