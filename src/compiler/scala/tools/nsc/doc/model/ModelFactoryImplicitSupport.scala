/* NSC -- new Scala compiler -- Copyright 2007-2012 LAMP/EPFL 
 *
 * This trait finds implicit conversions for a class in the default scope and creates scaladoc entries for each of them.
 *
 * @author Vlad Ureche
 * @author Adriaan Moors
 */

package scala.tools.nsc
package doc
package model

import comment._

import scala.collection._
import scala.util.matching.Regex

import symtab.Flags
import io._

import model.{ RootPackage => RootPackageEntity }

/** 
 * This trait finds implicit conversions for a class in the default scope and creates scaladoc entries for each of them.
 *
 * Let's take this as an example:
 * {{{
 *    object Test {
 *      class A
 *
 *      class B { 
 *        def foo = 1
 *      }
 *
 *      class C extends B { 
 *        def bar = 2 
 *        class D
 *      }
 *
 *      implicit def conv(a: A) = new C
 *    }
 * }}}
 *
 * Overview:
 * - scaladoc-ing the above classes, `A` will get two more methods: foo and bar, over its default methods
 * - the nested classes (specifically `D` above), abstract types, type aliases and constructor members are not added to 
 * `A` (see makeMember0 in ModelFactory, last 3 cases)
 * - the members added by implicit conversion are always listed under the implicit conversion, not under the class they
 * actually come from (`foo` will be listed as coming from the implicit conversion to `C` instead of `B`) - see 
 * `definitionName` in MemberImpl
 *
 * Internals:
 * TODO: Give an overview here
 */
trait ModelFactoryImplicitSupport {
  thisFactory: ModelFactory with CommentFactory with TreeFactory =>
  
  import global._
  import global.analyzer._
  import global.definitions._
  
  // debugging:
  val DEBUG: Boolean = System.getProperty("scaladoc.implicits.debug", "false") == "true"
  val ERROR: Boolean = System.getProperty("scaladoc.implicits.error", "false") == "true" || DEBUG
  @inline final def debug(msg: => String) = if (DEBUG) println(msg)
  @inline final def error(msg: => String) = if (ERROR) println(msg)
  
  /** This is a flag that indicates whether to resolve static implicits. For example, if an implicit conversion requires
   *  that there is a Numeric[T] in scope:
   *  {{{
   *     class A[T]
   *     class B extends A[Int]
   *     class C extends A[String]
   *     implicit def pimpA[T: Numeric](a: A[T]): D
   *  }}}
   *  If the flag is set, no constraints are generated for the conversion from B to D and the conversion from C to D
   *  is not generated at all, since there's no such implicit in scope
   */
  val eliminateImplicits: Boolean = System.getProperty("scaladoc.implicits.eliminate", "true") == "true"
  class ImplicitNotFound(tpe: Type) extends Exception("No implicit of type " + tpe + " found in scope.")

  /* ============== IMPLEMENTATION PROVIDING ENTITY TYPES ============== */  

  class ImplicitConversionImpl(
    val sym: Symbol,
    val convSym: Symbol,
    val toType: Type,
    val constrs: List[Constraint],
    inTpl: => DocTemplateImpl) 
      extends ImplicitConversion {

    def source: DocTemplateEntity = inTpl

    def targetType: TypeEntity = makeType(toType, inTpl)
    
    def convertorOwner: TemplateEntity = 
      if (convSym != NoSymbol) 
        makeTemplate(convSym.owner) 
      else {
        error("Implicit conversion from " + sym.tpe + " to " + toType + " done by " + convSym + " = NoSymbol!")
        makeRootPackage.get // surely the root package was created :)
      }
    
    def convertorMethod: Either[MemberEntity, String] = { 
      var convertor: MemberEntity = null

      convertorOwner match {
        case doc: DocTemplateImpl =>
          val convertors = members.collect { case m: MemberImpl if m.sym == convSym => m }
          if (convertors.length == 1)
            convertor = convertors.head
        case _ =>
      }
      if (convertor ne null) 
        Left(convertor)
      else
        Right(convSym.nameString)
    }

    def conversionQualifiedName = convertorOwner.qualifiedName + "." + convSym.nameString
    
    lazy val constraints: List[Constraint] = constrs
    
    val members: List[MemberEntity] = {
      // Obtain the members inherited by the implicit conversion
      val memberSyms = toType.members.filter(implicitShouldDocument(_))
      
      // Debugging part :)
      debug(sym.nameString + "\n" + "=" * sym.nameString.length())
      debug(" * conversion " + convSym + " from " + sym.tpe + " to " + toType)
      debug("   -> full type: " + toType)
      if (constraints.length != 0) {
        debug("   -> constraints: ")      
        constraints foreach { constr => debug("      - " + constr) }
      }
      debug("   -> members:")
      memberSyms foreach (sym => debug("      - "+ sym.decodedName +" : " + sym.info))
      debug("")
      
      memberSyms.flatMap((makeMember(_, this, inTpl)))
    }
  }
  
  /* ============== MAKER METHODS ============== */
  
  /**
   *  Make the implicit conversion objects
   *  
   *  A word about the scope of the implicit conversions: currently we look at a very basic context composed of the 
   *  default Scala imports (Predef._ for example) and the companion object of the current class, if one exists. In the 
   *  future we might want to extend this to more complex scopes.
   */
  def makeImplicitConversions(sym: Symbol, inTpl: => DocTemplateImpl): List[ImplicitConversion] =
    if (!(sym.isClass || sym.isTrait)) Nil
    else {
      val context: global.analyzer.Context = global.analyzer.rootContext(NoCompilationUnit)            
      val results = global.analyzer.allViewsFrom(sym.tpe, context, sym.typeParams)

      var conversions = results.flatMap(result => makeImplicitConversion(sym, result._1, result._2, context, inTpl))
      conversions = conversions.filterNot(_.members.isEmpty)
      
      // Put the class-specific conversions in front
      val (ownConversions, commonConversions) = 
        conversions.partition(conv => !hardcoded.commonConversionTargets.contains(conv.conversionQualifiedName))

      ownConversions ::: commonConversions
    }

  /** makeImplicitConversion performs the heavier lifting to get the implicit listing:
   * - for each possible conversion function (also called view) 
   *    * figures out the final result of the view (to what is our class transformed?)
   *    * figures out the necessary constraints on the type parameters (such as T <: Int) and the context (such as Numeric[T])
   *    * lists all inherited members
   * 
   * What? in details: 
   *  - say we start from a class A[T1, T2, T3, T4]
   *  - we have an implicit function (view) in scope: 
   *     def pimpA[T3 <: Long, T4](a: A[Int, Foo[Bar[X]], T3, T4])(implicit ev1: Manifest[T4], ev2: Numeric[T4]): PimpedA
   *  - A is converted to PimpedA ONLY if a couple of constraints are satisfied:
   *     * T1 must be equal to Int
   *     * T2 must be equal to Foo[Bar[X]]
   *     * T3 must be upper bounded by Long
   *     * there must be evidence of Numeric[T4] and a Mainfest[T4] within scope
   *  - the final type is PimpedA and A therefore inherits a couple of members from pimpedA
   *  
   * How?
   * some notes:
   *  - Scala's type inference will want to solve all type parameters down to actual types, but we only want constraints
   * to maintain generality
   *  - therefore, allViewsFrom wraps type parameters into "untouchable" type variables that only gather constraints,
   * but are never solved down to a type
   *  - these must be reverted back to the type parameters and the constraints must be extracted and simplified (this is
   * done by the uniteConstraints and boundedTParamsConstraints. Be sure to check them out
   *  - we also need to transform implicit parameters in the view's signature into constraints, such that Numeric[T4] 
   * appears as a constraint 
   */
  def makeImplicitConversion(sym: Symbol, result: SearchResult, constrs: List[TypeConstraint], context: Context, inTpl: => DocTemplateImpl): List[ImplicitConversion] =
    if (result.tree == EmptyTree) Nil
    else {
      // `result` will contain the type of the view (= implicit conversion method)
      // the search introduces untouchable type variables, but we want to get back to type parameters
      val viewFullType = result.tree.tpe
      // set the previously implicit parameters to being explicit

      val (viewSimplifiedType, viewImplicitTypes) = removeImplicitParameters(viewFullType)
      
      // TODO: Isolate this corner case :) - Predef.<%< and put it in the testsuite
      if (viewSimplifiedType.params.length != 1) {
        error("incorrect number of parameters: " + viewSimplifiedType.params.length)
        error("  type: " + viewSimplifiedType)
        error("  while transforming sym: " + sym)
        error("  with tree: " + result.tree)
        return Nil
      }

      // type the view application so we get the exact type of the result (not the formal type)
      val viewTree = result.tree.setType(viewSimplifiedType)
      val appliedTree = new ApplyImplicitView(viewTree, List(Ident("<argument>") setType viewTree.tpe.paramTypes.head))
      val appliedTreeTyped: Tree = {
        val newContext = context.makeImplicit(context.ambiguousErrors)
        val newTyper = global.analyzer.newTyper(newContext)
          newTyper.silent(_.typed(appliedTree, global.analyzer.EXPRmode, WildcardType), false) match {

          case global.analyzer.SilentResultValue(t: Tree) => t
          case global.analyzer.SilentTypeError(err) =>
            global.reporter.warning(sym.pos, err.toString)
            return Nil
        }
      }

      // now we have the final type:
      val toType = wildcardToNothing(typeVarToOriginOrWildcard(appliedTreeTyped.tpe.finalResultType))

      try {
        // Transform bound constraints into scaladoc constraints
        val implParamConstraints = makeImplicitConstraints(viewImplicitTypes, sym, context, inTpl)
        val boundsConstraints = makeBoundedConstraints(sym.typeParams, constrs, inTpl)
        // TODO: no substitution constraints appear in the library and compiler scaladoc. Maybe they can be removed?
        val substConstraints = makeSubstitutionConstraints(result.subst, inTpl)
        val constraints = implParamConstraints ::: boundsConstraints ::: substConstraints     
        
        List(new ImplicitConversionImpl(sym, result.tree.symbol, toType, constraints, inTpl))
      } catch {
        case i: ImplicitNotFound => 
          error("eliminating conversion from " + sym + " to " + toType + " because: " + i.toString)
          Nil
      }
    }
  
  def makeImplicitConstraints(types: List[Type], sym: Symbol, context: Context, inTpl: => DocTemplateImpl): List[Constraint] = 
    types.flatMap((tpe:Type) => {
      // TODO: Before creating constraints, map typeVarToOriginOrWildcard on the implicitTypes
      val implType = typeVarToOriginOrWildcard(tpe)
      val qualifiedName = implType.typeSymbol.ownerChain.reverse.map(_.nameString).mkString(".")
      
      var available: Option[Boolean] = None

      // look for type variables in the type. If there are none, we can decide if the implicit is there or not
      if (eliminateImplicits && typeVarsInType(tpe).isEmpty) {
        try {
          context.flushBuffer() /* any errors here should not prevent future findings */
          val search = inferImplicit(EmptyTree, tpe, false, false, context, false)
          context.flushBuffer() /* any errors here should not prevent future findings */
          available = Some(search.tree != EmptyTree)
        } catch {
          case _ =>
        }
      }

      available match {
        case Some(true) => 
          Nil
        case Some(false) => 
          throw new ImplicitNotFound(implType)
        case None =>
          val typeParamNames = sym.typeParams.map(_.name)

          // TODO: This is maybe the worst hack I ever did - it's as dirty as hell, but it seems to work, so until I
          // learn more about symbols, it'll have to do.
          implType match {
            case TypeRef(pre, sym, List(TypeRef(NoPrefix, targ, Nil))) if (typeParamNames contains targ.name) =>
              hardcoded.knownTypeClasses.get(qualifiedName) match {
                case Some(explanation) =>
                  List(new KnownTypeClassConstraint {
                    val typeParamName = targ.nameString
                    val typeExplanation = explanation
                    val typeClassEntity = makeTemplate(sym)
                    val implicitType: TypeEntity = makeType(implType, inTpl)
                  })
                case None =>
                  List(new TypeClassConstraint {
                    val typeParamName = targ.nameString
                    val typeClassEntity = makeTemplate(sym)
                    val implicitType: TypeEntity = makeType(implType, inTpl)
                  })
              }
            case _ =>
              List(new ImplicitInScopeConstraint{
                val implicitType: TypeEntity = makeType(implType, inTpl)
              })
          }
      }
    })

  def makeSubstitutionConstraints(subst: TreeTypeSubstituter, inTpl: => DocTemplateImpl): List[Constraint] =
    (subst.from zip subst.to) map {
      case (from, to) =>             
        new EqualTypeParamConstraint {
          error("Unexpected type substitution constraint from:" + from + " to: " + to)
          val typeParamName = from.toString
          val rhs = makeType(to, inTpl)
        }
    }
  
  def makeBoundedConstraints(tparams: List[Symbol], constrs: List[TypeConstraint], inTpl: => DocTemplateImpl): List[Constraint] = 
    (tparams zip constrs) flatMap {
      case (tparam, constr) => {
        uniteConstraints(constr) match {
          case (loBounds, upBounds) => (loBounds filter (_ != NothingClass.tpe), upBounds filter (_ != AnyClass.tpe)) match {
            case (Nil, Nil) => 
              Nil
            case (List(lo), List(up)) if (lo == up) =>
              List(new EqualTypeParamConstraint {
                val typeParamName = tparam.nameString
                val rhs = makeType(lo, inTpl)
              })
            case (List(lo), List(up)) =>
              List(new BoundedTypeParamConstraint {
                val typeParamName = tparam.nameString
                val lowerBound = makeType(lo, inTpl)
                val upperBound = makeType(up, inTpl)
              })
            case (List(lo), Nil) =>
              List(new LowerBoundedTypeParamConstraint {
                val typeParamName = tparam.nameString
                val lowerBound = makeType(lo, inTpl)
              })
            case (Nil, List(up)) =>
              List(new UpperBoundedTypeParamConstraint {
                val typeParamName = tparam.nameString
                val upperBound = makeType(up, inTpl)
              })
            case other =>
              // this is likely an error on the lub/glb side
              error("error computing lub/glb for: " + (tparam, constr) + ":\n" + other)
              Nil
          }
        }
      }
    }

  /**
   * uniteConstraints takes a TypeConstraint instance and simplifies the constraints inside
   *  
   * Normally TypeConstraint contains multiple lower and upper bounds, and we want to reduce this to a lower and an 
   * upper bound. Here are a couple of catches we need to be aware of:
   *  - before finding a view (implicit method in scope that maps class A[T1,T2,.. Tn] to something else) the type
   * parameters are transformed into "untouchable" type variables so that type inference does not attempt to 
   * fully solve them down to a type but rather constrains them on both sides just enough for the view to be 
   * applicable -- now, we want to transform those type variables back to the original type parameters
   *  - some of the bounds fail type inference and therefore refer to Nothing => when performing unification (lub, glb)
   * they start looking ugly => we (unsoundly) transform Nothing to WildcardType so we fool the unification algorithms
   * into thinking there's nothing there
   *  - we don't want the wildcard types surviving the unification so we replace them back to Nothings
   */
  def uniteConstraints(constr: TypeConstraint): (List[Type], List[Type]) =
    try {
      (List(wildcardToNothing(lub(constr.loBounds map typeVarToOriginOrWildcard))),
       List(wildcardToNothing(glb(constr.hiBounds map typeVarToOriginOrWildcard))))
    } catch { 
      // does this actually ever happen? (probably when type vars occur in the bounds)  
      case x: Throwable => (constr.loBounds.distinct, constr.hiBounds.distinct) 
    } 

  /**
   *  Make implicits explicit - Not used curently
   */
  object implicitToExplicit extends TypeMap {
    def apply(tp: Type): Type = mapOver(tp) match {         
      case MethodType(params, resultType) =>
        MethodType(params.map(param => if (param.isImplicit) param.cloneSymbol.resetFlag(Flags.IMPLICIT) else param), resultType)
      case other =>
        other       
    }
  }

  /** 
   * removeImplicitParameters transforms implicit parameters from the view result type into constraints and 
   * returns the simplified type of the view
   *  
   * for the example view:
   *   implicit def pimpMyClass[T](a: MyClass[T])(implicit ev: Numeric[T]): PimpedMyClass[T]
   * the implicit view result type is:
   *   (a: MyClass[T])(implicit ev: Numeric[T]): PimpedMyClass[T] 
   * and the simplified type will be:
   *   MyClass[T] => PimpedMyClass[T]
   */
  def removeImplicitParameters(viewType: Type): (Type, List[Type]) = {

    val params = viewType.paramss.flatten
    val (normalParams, implParams) = params.partition(!_.isImplicit)
    val simplifiedType = MethodType(normalParams, viewType.finalResultType)
    val implicitTypes = implParams.map(_.tpe)

    (simplifiedType, implicitTypes)
  }
    
  /**
   * typeVarsToOriginOrWildcard transforms the "untouchable" type variables into either their origins (the original
   * type parameters) or into wildcard types if nothing matches
   */
  object typeVarToOriginOrWildcard extends TypeMap {
    def apply(tp: Type): Type = mapOver(tp) match {         
      case tv: TypeVar =>
        if (tv.constr.inst.typeSymbol == NothingClass)
          WildcardType
        else
          tv.origin //appliedType(tv.origin.typeConstructor, tv.typeArgs map this)
      case other =>
        if (other.typeSymbol == NothingClass)
          WildcardType
        else
          other       
    }
  }

  /**
   * wildcardToNothing transforms wildcard types back to Nothing
   */
  object wildcardToNothing extends TypeMap {
    def apply(tp: Type): Type = mapOver(tp) match {         
      case WildcardType =>
        NothingClass.tpe
      case other =>
        other       
    }
  }

  /** implicitShouldDocument decides whether a member inherited by implicit conversion should be documented */  
  def implicitShouldDocument(aSym: Symbol): Boolean = {
    // We shouldn't document:
    // - constructors
    // - common methods (in Any, AnyRef, Object) as they are automatically removed
    // - private and protected members (not accessible following an implicit conversion)
    // - members starting with _ (usually reserved for internal stuff)
    localShouldDocument(aSym) && (!aSym.isConstructor) && (aSym.owner != ObjectClass) && 
    (aSym.owner != AnyClass) && (aSym.owner != AnyRefClass) && 
    (!aSym.isProtected) && (!aSym.isPrivate) && (!aSym.name.toString.startsWith("_"))
  }
}

object hardcoded {
  /** Common conversion targets that affect any class in Scala */
  val commonConversionTargets = 
    List("scala.Predef.any2stringfmt", "scala.Predef.any2stringadd", "scala.Predef.any2ArrowAssoc", "scala.Predef.any2Ensuring")

  /** The common conversions and the text */
  val knownTypeClasses: Map[String, String] = Map() +
    ("<root>.scala.package.Numeric"       -> "is a numeric class, such as Int, Long, Float or Double") +
    ("<root>.scala.package.Integral"      -> "is an integral numeric class, such as Int or Long") +
    ("<root>.scala.package.Fractional"    -> "is a fractional numeric class, such as Float or Double") +
    ("<root>.scala.reflect.Manifest"      -> "is accompanied by a Manifest, which is a runtime representation of its type that survives erasure") + 
    ("<root>.scala.reflect.ClassManifest" -> "is accompanied by a ClassManifest, which is a runtime representation of its type that survives erasure") + 
    ("<root>.scala.reflect.OptManifest"   -> "is accompanied by an OptManifest, which can be either a runtime representation of its type or the NoManifest, which means the runtime type is not available")
}