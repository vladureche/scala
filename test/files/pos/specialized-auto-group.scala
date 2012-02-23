
class C[@specialized((Int, Long, Unit)) T] {
  def f: T = sys.error("not implemented")
}
