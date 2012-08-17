// This test verifies that even for semantically incorrect code, the closure
// elimination phase continues to generate correct java bytecode.
//
// The sequence in question:
//   LOAD_LOCAL v1  // <= loads v1: Long onto the stack
//   BOX LONG       // <= takes a Long, returns an Object
//   UNBOX INT      // <= takes an Object, returns an Int
//
// From a Scala perspective, this can't happen. But with aggresive inline, 
// there is a way to produce this code. It won't ever be executed directly
// and if expressly called, should result in a runtime exception. But with
// the closure elimination (better called copy propagation phase) this
// code will result in an incorrect:
//
//   LOAD_LOCAL v1
//   BOX LONG
//   LOAD_LOCAL v1  // <= an Int should be on the stack, but we get a 
//                  // 2-slot long
// 
// This test checks that no such change happens 
package scala {
  trait Function1[@specialized(scala.Long) -T1, @specialized(scala.Unit) +R] extends AnyRef { self =>
    def apply(v1: T1): R
  }
}

package test {
  abstract class SL[A] extends Function1[Int, A] { self =>
    def apply(idx: Int): A
    def view = new Function1[Int, A] {
      def apply(idx: Int) = self.apply(idx)
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = 
    println((new test.SL[String] { def apply(idx: Int): String = "hello" }).view(0))
}
