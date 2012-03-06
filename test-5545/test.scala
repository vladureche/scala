/*
trait F[@specialized(Int) T1, @specialized(AnyRef) R] {
  def f(v1: T1): R
  def g = (v1: T1) => f(v1)
}
*/

trait F[@specialized(Int) T1, @specialized(Int) T2, @specialized(Int,AnyRef) R] {
  def apply(v1: T1, v2: T2): R

  def tupled: Tuple2[T1, T2] => R = {
    case Tuple2(x1, x2) => apply(x1, x2)
  }
}

