/*
 * Dear Inliner,
 *
 * I write to you regarding recent rumors that you don't want to inline
 * calls in our beloved object Predef. I would argue that is nonsense
 * and I dare say, even malice on other people's behalf. But since
 * recently reports intensified, we see ourselves forced to investigate
 * the issue.
 * The Scalac committee thereby decided you need to be tested on each
 * run. I apologize for any inconvenience this might cause on your side
 * and I want to express my confidence in you and my gratitude for the
 * work you are doing inside the Scalac compiler. Thank you!
 *
 * Yours faithfully, 
 * The Test suite
 */
import reflect.ClassTag

object Test {
  assert(0 == 1, "<inline me>")
  implicitly[ClassTag[String]]
  1 -> 2  
}
