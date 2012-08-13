// The inliner would inline this infinitely
// unless it had some heuristic to stop
// taken from the compiler crash caused by the
// scala.collection.immutable.Stream :)
class InfiniteInline {

  // this is the trigger call
  // which will receive the infinite inlining
  def trigger: String =
    recurse1(x => x)

  // this is the call that will keep being inlined - 
  // not directly, as recursive functions are not inlined
  // but through recurse2
  @inline final def recurse1(f: String => String): String =
    "<" + f(recurse2(f)) + ">"

  @inline final def recurse2(f: String => String): String =
    "<" + f(recurse1(f)) + ">"

}
