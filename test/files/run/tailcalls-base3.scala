// This test checks correct handling of lazy values in tailcalls-transformed functions
object Test extends App {
  var i = 0
  @annotation.tailrec
  def iterate(str: String): String = {
    lazy val add: String = i.toString
    if (i < 5) { i = i + 1; iterate(str + " " + add) }
    else str
  }
  println(iterate("iteration:"))
}
