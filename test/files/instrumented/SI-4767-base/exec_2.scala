import scala.tools.partest.instrumented.Instrumentation._
import instrumented._

object Test {
  def main(args: Array[String]) {
    // force predef initialization before profiling
    Predef
    // create instances of Foo and Bar
    val foo = new Foo {}
    val bar = new Bar
    startProfiling()
    // if the inlining took place, you won't see a call to Foo$class.foo, meaning it wasn't inlined
    bar.bar(foo)
    stopProfiling()
    printStatistics()
  }
}

