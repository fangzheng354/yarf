package vagueobjects.rf

import vagueobjects.rf.construction.Sampler

class DefaultSampler(numRows:Int) extends Sampler{
  override def sample()  = ((0 until numRows).toArray, Array.empty[Int])
}
