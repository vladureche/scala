import scala.tools.partest.instrumented.Instrumentation._
import instrumented._

object Test {
  def main(args: Array[String]) {
    // force predef initialization before profiling
    Predef
    val a = new A {}
    val b = new B
    startProfiling()
    // if the inlining takes place, you won't see any calls to A$class.foo here
    b.bar(a)
    stopProfiling()
    printStatistics()
  }
}

