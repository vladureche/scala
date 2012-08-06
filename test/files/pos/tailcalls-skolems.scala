// This test the correct types are replaced in the tailcalls phase
object TestSkolems {
  @annotation.tailrec
  final def foo(x: Any): Unit = x match {
   case s: String => println(s)
   case _ => foo(x)
  }
}
