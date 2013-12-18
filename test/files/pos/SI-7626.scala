class C[@specialized(Int) T](c: T) {
  def test(t: T): Any = {
    class D[U](d: U)
    new D(c)
  }
}
