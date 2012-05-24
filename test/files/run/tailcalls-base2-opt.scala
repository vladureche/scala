// This test checks correct handling of the this pointer in the tailcalls transformation
// during development, the closure elimination phase broke this test
// see SI-6249 for details
trait Base {
  def message: String
  class T(i: Int) { println(message) ; def msg = message }

  @annotation.tailrec final /* make sure it's transformed by tailcalls */
  def tailCall(a:Int, other: Base): T =
    if (a == 0) 
      new T(0)
    else 
      other.tailCall(a-1, this) // tail call-transformed
}

class ReturnsInt extends Base { def message = this.getClass.toString }
class ReturnsStr extends Base { def message = this.getClass.toString }

object Test extends App {
  val rInt = new ReturnsInt
  val rStr = new ReturnsStr

  for (x <- 1 to 10)
    println(rInt.tailCall(x, rStr).msg)
}
