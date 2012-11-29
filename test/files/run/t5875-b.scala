import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test {
  def main(args: Array[String]): Unit = {
    def z(x: Int) = x
    reify(z(1))
  }
}
