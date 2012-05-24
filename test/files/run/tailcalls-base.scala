// This test checks correct passing of the 'this' pointer between tail-recursive invocations
class Foo {
  def foo = 3

  @annotation.tailrec
  final def bar(a: Int, acc: Int, other: Foo):Int = { 
    println(a + " => " + foo)
    if (a == 1) 
      acc
    else 
      other.bar(a-1, a * acc, this) 
  }
}

class Bar extends Foo {
  override def foo = 4
}

object Test extends App {
  val foo = new Foo
  val bar = new Bar
  foo.bar(3, 1, bar)
}
