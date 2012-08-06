// original testcase
trait Foo[@specialized(Int) A] {
  final def bar(a:A):A = bar(a)
}

// another testcase that I was using
trait Test {
  trait B[T]
  private final def grow[T](): B[T] = grow[T]()
}
