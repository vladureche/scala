package scala.reflect
package api

/** A mirror-aware factory for trees.
 *
 * This class is used internally by Scala Reflection, and is not recommended for use in client code.
 */
abstract class TreeCreator {
  def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U # Tree
}
