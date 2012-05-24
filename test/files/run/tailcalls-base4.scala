// This test checks correct tailcalls transformation for curried methods
trait Base {
  def message: String
  class T(i: Int) { println(message) ; def msg = message }

  @annotation.tailrec final /* make sure it's transformed by tailcalls */
  def tailCall(a:Int, other: Base)(x: Int)(implicit y: Int): T =
    if (a == 0) 
      new T(0)
    else 
      other.tailCall(a-1, this)(x) // tail call-transformed
}

class ReturnsInt extends Base { def message = this.getClass.toString }
class ReturnsStr extends Base { def message = this.getClass.toString }

object Test extends App {
  val rInt = new ReturnsInt
  val rStr = new ReturnsStr

  for (x <- 1 to 10)
    println(rInt.tailCall(x, rStr)(1)(2).msg)
}
