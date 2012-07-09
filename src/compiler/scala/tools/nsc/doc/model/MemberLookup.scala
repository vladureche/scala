package scala.tools.nsc
package doc
package model

import comment._

import diagram._

import scala.collection._
import scala.util.matching.Regex

import symtab.Flags

import io._

import model.{ RootPackage => RootPackageEntity }

/** This trait extracts all required information for documentation from compilation units */
trait MemberLookup {
  thisFactory: ModelFactory =>

  import global._

  def memberLookup(pos: Position, query: String, inTpl: DocTemplateImpl): LinkTo = {
    assert(modelFinished)

    // (1) Lookup in the root package, as most of the links are qualified
    var linkTo: LinkTo = lookupInRootPackage(pos, query)

    // (2) Recursively go into each
    var currentTpl = inTpl
    while (!currentTpl.isRootPackage && (linkTo == NoLink)) {
      linkTo = lookupInTemplate(pos, query, currentTpl)
      currentTpl = currentTpl.inTemplate
    }

    // (3) Look at external links
    if (linkTo == NoLink) {
      // TODO: IF THIS IS THE ROOT PACKAGE, LOOK AT EXTERNAL LINKS
    }

    // (4) Last resort: if we still haven't found anything, create a tooltip
    if (linkTo == NoLink)
      Tooltip(query)
    else
      linkTo
  }

  def lookupInRootPackage(pos: Position, query: String) = lookupInTemplate(pos, query, makeRootPackage)
  def lookupInTemplate(pos: Position, query: String, inTpl: DocTemplateImpl): LinkTo = {
    // TODO: Implement this!
    NoLink
  }
}