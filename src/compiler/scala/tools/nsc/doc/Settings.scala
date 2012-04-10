/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package doc

import java.io.File
import java.lang.System

/** An extended version of compiler settings, with additional Scaladoc-specific options.
  * @param error A function that prints a string to the appropriate error stream. */
class Settings(error: String => Unit) extends scala.tools.nsc.Settings(error) {

  /** A setting that defines in which format the documentation is output. ''Note:'' this setting is currently always
    * `html`. */
  val docformat = ChoiceSetting (
    "-doc-format",
    "format",
    "Selects in which format documentation is rendered",
    List("html"),
    "html"
  )

  /** A setting that defines the overall title of the documentation, typically the name of the library being
    * documented. ''Note:'' This setting is currently not used. */
  val doctitle = StringSetting (
    "-doc-title",
    "title",
    "The overall name of the Scaladoc site",
    ""
  )

  /** A setting that defines the overall version number of the documentation, typically the version of the library being
    * documented. ''Note:'' This setting is currently not used. */
  val docversion = StringSetting (
    "-doc-version",
    "version",
    "An optional version number, to be appended to the title",
    ""
  )

  val docfooter = StringSetting (
    "-doc-footer",
    "footer",
    "A footer on every ScalaDoc page, by default the EPFL/Typesafe copyright notice. Can be overridden with a custom footer.",
    ""
  )

  val docUncompilable = StringSetting (
    "-doc-no-compile",
    "path",
    "A directory containing sources which should be parsed, no more (e.g. AnyRef.scala)",
    ""
  )

  lazy val uncompilableFiles = docUncompilable.value match {
    case ""     => Nil
    case path   => io.Directory(path).deepFiles filter (_ hasExtension "scala") toList
  }

  /** A setting that defines a URL to be concatenated with source locations and show a link to source files.
   * If needed the sourcepath option can be used to exclude undesired initial part of the link to sources */
  val docsourceurl = StringSetting (
    "-doc-source-url",
    "url",
    "A URL pattern used to build links to template sources; use variables, for example: ?{TPL_NAME} ('Seq'), ?{TPL_OWNER} ('scala.collection'), ?{FILE_PATH} ('scala/collection/Seq')",
    ""
  )

  val useStupidTypes = BooleanSetting (
    "-Yuse-stupid-types",
    "Print the types of inherited members as seen from their original definition context. Hint: you don't want to do that!"
  )

  val docgenerator = StringSetting (
    "-doc-generator",
    "class-name",
    "The fully qualified name of a doclet class, which will be used to generate the documentation",
    "scala.tools.nsc.doc.html.Doclet"
  )

  val docRootContent = PathSetting (
    "-doc-root-content",
    "The file from which the root package documentation should be imported.",
    ""
  )

  val docImplicits = BooleanSetting (
    "-implicits",
    "Document members inherited by implicit conversions."
  )

  val docImplicitsDebug = BooleanSetting (
    "-implicits-debug",
    "Show debugging information for members inherited by implicit conversions."
  )

  val docImplicitsShowAll = BooleanSetting (
    "-implicits-show-all",
    "Show members inherited by implicit conversions that are impossible in the default scope. " +
    "(for example conversions that require Numeric[String] to be in scope)"
  )

  val docDiagrams = BooleanSetting (
    "-diagrams",
    "Create inheritance diagrams for classes, traits and packages."
  )

  val docDiagramsDebug = BooleanSetting (
    "-diagrams-debug",
    "Show debugging information for the diagram creation process."
  )

  val docDiagramsDotPath = PathSetting (
    "-diagrams-dot-path",
    "The path to the dot executable used to generate the inheritance diagrams. Ex: /usr/bin/dot",
    "dot" // by default, just pick up the system-wide dot
  )

  // Somewhere slightly before r18708 scaladoc stopped building unless the
  // self-type check was suppressed.  I hijacked the slotted-for-removal-anyway
  // suppress-vt-warnings option and renamed it for this purpose.
  noSelfCheck.value = true

  // For improved help output.
  def scaladocSpecific = Set[Settings#Setting](
    docformat, doctitle, docfooter, docversion, docUncompilable, docsourceurl, docgenerator, docRootContent, useStupidTypes,
    docDiagrams, docDiagramsDebug, docDiagramsDotPath,
    docImplicits, docImplicitsDebug, docImplicitsShowAll
  )
  val isScaladocSpecific: String => Boolean = scaladocSpecific map (_.name)

  override def isScaladoc = true

  /** 
   * Set of classes to exclude from index and diagrams 
   * TODO: Should be configurable, not hardcoded for the Scala Library
   */
  def isExcluded(qname: String) = {
    ( ( qname.startsWith("scala.Tuple") || qname.startsWith("scala.Product") ||
       qname.startsWith("scala.Function") || qname.startsWith("scala.runtime.AbstractFunction")
     ) && !(
      qname == "scala.Tuple1" || qname == "scala.Tuple2" ||
      qname == "scala.Product" || qname == "scala.Product1" || qname == "scala.Product2" ||
      qname == "scala.Function" || qname == "scala.Function1" || qname == "scala.Function2" ||
      qname == "scala.runtime.AbstractFunction0" || qname == "scala.runtime.AbstractFunction1" ||
      qname == "scala.runtime.AbstractFunction2"
    )
   )
  }
}
