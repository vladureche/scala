// This test checks that the types are replaced correctly in the tailcalls phase
// it failed at some point during development
object TestSkolems {
  @annotation.tailrec
  final def foo(x: Any): Unit = x match {
   case s: String => println(s)
   case _ => foo(x)
  }
}
