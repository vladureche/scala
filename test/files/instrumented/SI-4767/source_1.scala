package instrumented

/** Foo is a trait with a final method, that could be inlined */
trait Foo {
  @inline final def foo(x: Int): Unit = {
    println(x)
  }
}

/** Bar is the possible inline target */
class Bar {
  def bar(y: Foo): Unit = {
    y.foo(7)
  }
}
