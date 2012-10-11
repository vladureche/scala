package scala.reflect

import scala.reflect.api.{Universe => ApiUniverse}

/** The Scala Reflection API (located in scala-reflect.jar).
 *
 * In Scala 2.10.0, the Scala Reflection API and its implementation have an "experimental" status.
 * This means that the API and the docs are not complete and can be changed in binary- and source-incompatible
 * manner in 2.10.1. This also means that the implementation has some known issues.
 *
 * The following types are the backbone of the Scala Reflection API, and serve as a good starting point
 * for information about Scala Reflection:
 *
 *  - [[scala.reflect.api.Symbols]]
 *  - [[scala.reflect.api.Types]]
 *  - [[scala.reflect.api.Mirrors]]
 *  - [[scala.reflect.api.Universe]]
 *
 *  For more information about Scala Reflection, see the 
 * [[http://docs.scala-lang.org/overviews/reflection/overview.html Reflection Guide]]
 *
 */
package object api {

  // anchors for materialization macros emitted during tag materialization in Implicits.scala
  // implementation is hardwired into `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  // todo. once we have implicit macros for tag generation, we can remove these anchors
  private[scala] def materializeWeakTypeTag[T](u: ApiUniverse): u.WeakTypeTag[T] = ??? // macro
  private[scala] def materializeTypeTag[T](u: ApiUniverse): u.TypeTag[T] = ??? // macro
}