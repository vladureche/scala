// This test checks the correct handling of the this's type in the tailcalls transformation
trait TestThis {
  trait B
  @annotation.tailrec
  private final def grow(): B = grow()
}
