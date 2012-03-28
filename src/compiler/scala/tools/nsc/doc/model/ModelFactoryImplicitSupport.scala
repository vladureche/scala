/* NSC -- new Scala compiler -- Copyright 2007-2011 LAMP/EPFL */

package scala.tools.nsc
package doc
package model

import comment._

import scala.collection._
import scala.util.matching.Regex

import symtab.Flags
import io._

import model.{ RootPackage => RootPackageEntity }

/** This trait extracts all required information for documentation from compilation units */
trait ModelFactoryImplicitSupport {
  thisFactory: ModelFactory with CommentFactory with TreeFactory =>
  
  import global._
  import definitions.{ ObjectClass, RootPackage, EmptyPackage, NothingClass, AnyClass, AnyValClass, AnyRefClass }
  import model.comment.Block // shadow global's Block
  
  // debugging:
  val DEBUG: Boolean = System.getProperty("SCALADOC_IMPLICIT_DEBUG", "false") == "true"

  @inline
  def debug(msg: => String) =
    if (DEBUG)
      println(msg)
  
  /* ============== IMPLEMENTATION PROVIDING ENTITY TYPES ============== */  

  class ImplicitConversionImpl(convertorMethodSymbol: Symbol,
                               targetType: Type,
                               val constraints: List[ConstraintEntity],
                               inTpl: => TemplateImpl) extends ImplicitConversion {
    def target: TypeEntity = makeType(targetType, inTpl)
    def convertorOwner: TemplateEntity = makeTemplate(convertorMethodSymbol.owner)
    def convertorMethod: Either[MemberEntity, String] = convertorOwner match {
      case doc: DocTemplateImpl =>
        // there might be multiple entities or none at all, as the makeMember function can return any number of entities
        val allTemplates = doc.membersMap flatMap { case (sym, entity) => if (sym == convertorMethodSymbol) List(entity) else Nil }
        if (allTemplates.length > 0) Left(allTemplates.head) else Right(convertorMethodSymbol.name.toString)
      case _ => Right(convertorMethodSymbol.name.toString)
    }
    // TODO: Get this out of here, it's not in the right place!
    def getBody: Body = {
      val header = Paragraph(Bold(Chain(Text("Member inherited by implicit conversion to ")::Monospace(Text(target.toString()))::Text(" by ")::Monospace(Text(convertorMethodSymbol.name.toString))::Text(" in ")::Monospace(Text(convertorMethodSymbol.owner.name.toString))::Text(".")::Nil)))
      val constrs: List[Block] = constraints.length match {
        case 0 => Nil
        case 1 => List(Paragraph(Text("The implicit conversion will take place only if: ")), UnorderedList(constraints map { _.getConstraintText }))
        case _ => List(Paragraph(Text("The implicit conversion will take place only if all the constraints are satisfied:")), UnorderedList(constraints map { _.getConstraintText }))
      }
      Body(header :: constrs ::: HorizontalRule() :: Nil)
    }
  }

  // TODO: Get constraint text out of here, it's not in the right place!
  class ImplicitInScopeConstraint(sym: Symbol, inTpl: => TemplateImpl) extends ConstraintEntity {
    def getConstraintText: Block = Paragraph(Chain(Text("An implicit value of type ")::Monospace(Text(sym.tpe.toString))::Text(" is in scope.")::Nil))
    override def toString = "An implicit value of type " + sym.tpe.toString + " is in scope"
  }

  class BoundConstraint(tp: Type, ub: Type, lb: Type, inTpl: => TemplateImpl) extends ConstraintEntity {
    def getConstraintText: Block = Paragraph(Chain(Text(tp + " is bounded by " + lb + " and " + ub + ": ") :: Monospace(Text(tp + " >: " + lb + " <: " + ub))::Nil)) 
    override def toString = tp + " >: " + lb + " <: " + ub
  }
  
  class EqualityConstraint(tp: Type, tp2: Type, inTpl: => TemplateImpl) extends ConstraintEntity {
    def getConstraintText: Block = Paragraph(Chain(Text(tp + " is equal to " + tp2 + ": ")::Monospace(Text(tp + " =: " + tp2))::Nil))
    override def toString = tp + " =: " + tp2
  }

  class UpperBoundedConstraint(tp: Type, ub: Type, inTpl: => TemplateImpl) extends ConstraintEntity {
    def getConstraintText: Block = Paragraph(Chain(Text(tp + " is upper bounded by " + ub + ": ")::Monospace(Text(tp + " <: " + ub))::Nil))
    override def toString = tp + " <: " + ub
  }
  
  class LowerBoundedConstraint(tp: Type, lb: Type, inTpl: => TemplateImpl) extends ConstraintEntity {
    def getConstraintText: Block = Paragraph(Chain(Text(tp + " is lower bounded by " + lb + ": ")::Monospace(Text(tp + " >: " + lb))::Nil))
    override def toString = tp + " >: " + lb
  }

  class ComplexBoundsConstraint(tp: Type, constr: TypeConstraint, inTpl: => TemplateImpl) extends ConstraintEntity {
    def getConstraintText: Block = Paragraph(Chain(Text("Complex constraint: ")::Monospace(Text(tp + " " + constr))::Nil))
    override def toString = "Complex constraint: " + constr
  }

  class SubstitutionConstraint(from: Symbol, to: Type, inTpl: => TemplateImpl) extends ConstraintEntity {
    def getConstraintText: Block = Paragraph(Chain(Text("Substitute type of ")::Monospace(Text(from.toString))::Text(" to ")::Monospace(Text(to.toString))::Nil))
    override def toString = "Type substitution: " + from + " to " + to
  }

  
  /* ============== MAKER METHODS ============== */
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
  
  /** membersByImplicitConversion returns all members obtained by implicit conversions to something else 
   * 
   *  A word about the scope of the implicit conversions: currently we look at a very basic context composed of the 
   *  default Scala imports (Predef._ for example) and the companion object of the current class, if one exists. In the 
   *  future we might want to extend this to more complex scopes.
   */
  def membersByImplicitConversions(sym: Symbol, inTpl: => TemplateImpl): List[(Symbol, ImplicitConversion)] = {
    if (!(sym.isClass || sym.isTrait))
      Nil
    else {
      debug("\n\n" + sym.nameString + "\n" + "=" * sym.nameString.length())
      
      val context: global.analyzer.Context = global.analyzer.rootContext(NoCompilationUnit)            
      val result = global.analyzer.allViewsFrom(sym.tpe, context, sym.typeParams)
      
      result flatMap { case (result, constr) => getMembersSymbols(sym, sym.tpe, context, sym.typeParams, result, constr, inTpl) }
    }
  }
  
  /** getMembersSymbols performs the heavier lifting to get the implicit listing:
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
  def getMembersSymbols(sym: Symbol,
                        tp: Type, 
                        context: global.analyzer.Context, 
                        tpars: List[Symbol], 
                        res: global.analyzer.SearchResult, 
                        constrs: List[TypeConstraint], 
                        inTpl: => TemplateImpl): List[(Symbol, ImplicitConversion)] = {

    if (res.tree == EmptyTree)
      Nil
    else {
    
      val (simplifiedType, implicitParamConstraints) = removeImplicitParameters(res.tree.tpe, inTpl)
      val coercion = res.tree.setType(simplifiedType)   
    
      // // this is weird: we might get a coercion with no type params?!? maybe it's 100% implicit and we explicitly apply it
      // // TODO: Add this to the testsuite :)
      // if (coercion.tpe.paramTypes.length != 1) {
      //   debug("no parameters:")
      //   deubg(simplifiedType)
      //   return Nil
      // }

      // and get the view applied to an argument
      val viewApply = new ApplyImplicitView(coercion, List(Ident("<argument>") setType coercion.tpe.paramTypes.head))
      val typed: Tree = 
        global.analyzer.newTyper(context.makeImplicit(context.ambiguousErrors)).
          silent(_.typed(viewApply, global.analyzer.EXPRmode, WildcardType), false) match {

          case global.analyzer.SilentResultValue(t: Tree) => t
          case global.analyzer.SilentTypeError(err) =>
            global.reporter.warning(sym.pos, err.toString)
            coercion
        }
  
      // the type vars need to be propagated until we ask for the members, then you can replace them by some simplified representation
      // in principle, we should solve the set of type variables, but this is unlikely to work since we can't really know any of them concretely
      // for now, just simplify them, and if their upper bounds =:= lower bounds, replace by that type,
      // else, if the lub and the glb could be computed, use an existential with the given lower and upper bound
      // if all that fails, you'll need some textual representation of the TypeConstraint
      val toType = typeVarToOriginOrWildcard(typed.tpe.finalResultType)
      val fullTpe = wildcardToNothing(typeVarToOriginOrWildcard(typed.tpe))

      // Transform bound constraints into scaladoc constraints
      val boundConstraints = boundedTParamsConstraints(tpars, constrs, inTpl) 
      val substConstraints = (res.subst.from zip res.subst.to) map { case (from, to) => new SubstitutionConstraint(from, to, inTpl) }    
      val constraints = implicitParamConstraints ::: boundConstraints ::: substConstraints     
       
      // Obtain the members inherited by the implicit conversion
      val implicitMembers = toType.nonPrivateMembers. 
                              filter(implicitShouldDocument(_)).
                              map { symbol => cloneSymbol(symbol).setInfo(toType memberInfo symbol) }
      
      // Debugging part :)
      debug(" * conversion "+ typed.symbol +" from "+ tp +" to "+ toType)
      debug("   -> full type: " + fullTpe)
      if (constraints.length != 0) {
        debug("   -> constraints: ")      
        constraints foreach { constr => debug("      - " + constr) }
      }
      debug("   -> members:")
      implicitMembers foreach (sym => debug("      - "+ sym.decodedName +" : " + sym.info))
      debug("")
          
      // Create the implicit conversion object
      val implicitConversion = new ImplicitConversionImpl(res.tree.symbol, toType, constraints, inTpl)
      
      implicitMembers.map((_, implicitConversion))
    }
  }

  /*
   * Clone the symbol and its comment. Also reset the DEFERRED flag. 
   *
   * Resetting the DEFERRED flag is a little trick here for refined types: (example from scala.collections)
   * {{{
   *     implicit def traversable2ops[T](t: collection.GenTraversableOnce[T]) = new TraversableOps[T] {
   *       def isParallel = ...
   * }}}
   *
   * the type the method returns is TraversableOps, which has all-abstract symbols. But in reality, it couldn't have
   * any abstract terms, otherwise it would fail compilation. So we reset the DEFERRED flag.
   */
  def cloneSymbol(symbol: Symbol): Symbol = {
    val sym = symbol.cloneSymbol.resetFlag(Flags.DEFERRED)
    val symComment = docComments.get(symbol)
    if (symComment.isDefined)
      docComments.put(sym, symComment.get)
    sym
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
  def uniteConstraints(constr: TypeConstraint): TypeConstraint =
    try {
      new TypeConstraint(List(wildcardToNothing(lub(constr.loBounds  map typeVarToOriginOrWildcard))), 
                         List(wildcardToNothing(glb(constr.hiBounds  map typeVarToOriginOrWildcard)))) 
    } catch { 
      // does this actually ever happen? (probably when type vars occur in the bounds)  
      case x: Throwable => new TypeConstraint(constr.loBounds.distinct, constr.hiBounds.distinct) 
    } 

  /** 
   * removeImplicitParameters transforms implicit parameters from the view result type into constraints and 
   * returns the simplified type of the view
   *  
   * for the example view:
   *   implicit def pimpMyClass[T](a: MyClass[T])(implicit ev: Numeric[T]): PimpedMyClass[T]
   * the implicit view result type is:
   *   (implicit ev: Numeric[T]): PimpedMyClass[T] 
   * and implicitParametersToConstraints will output a single constraint:
   *   ImplicitInScopeConstraint(ev: Numeric[T])
   * and will output the simplified type:
   *   MyClass[T] => PimpedMyClass[T]
   */
  def removeImplicitParameters(ty: Type, inTpl: => TemplateImpl): (Type, List[ConstraintEntity]) = ty match {
    case MethodType(params, resultType) if (params.forall(!_.isImplicit)) =>
      val (result, newConstraints) = removeImplicitParameters(typeVarToOriginOrWildcard(resultType), inTpl)
      (MethodType(params, result), newConstraints)
    case MethodType(params, resultType) =>
      val constraints = params.map(param => new ImplicitInScopeConstraint(param, inTpl))
      val (result, newConstraints) = removeImplicitParameters(typeVarToOriginOrWildcard(resultType), inTpl)
      (result, constraints:::newConstraints)
    case other =>
      (other, Nil)
  }   
  
  /**
   * boundedTParamsConstraints creates the constraints for the type parameters of the implicitly converted class
   */
  def boundedTParamsConstraints(tpars: List[Symbol], constrs: List[TypeConstraint], inTpl: => TemplateImpl) = 
    (tpars zip (constrs map uniteConstraints)) flatMap {
      case (param, constr) => {
        val tp = param.tpe
        val result = 
        (constr.loBounds, constr.hiBounds) match {          
          case (List(lb), List(ub)) if ((lb == NothingClass.tpe) && (ub == AnyClass.tpe)) => // Most generic bounds 
            Nil
          case (Nil, Nil) =>                                       // Empty bounds
            Nil
          case (List(lb), List(ub)) if (lb == ub) =>               // Same bound on both sides => equality            
            List(new EqualityConstraint(tp, lb, inTpl))
          case (List(lb), list) if (list == Nil || list == List(AnyClass.tpe)) =>     // Only lower bound
            List(new LowerBoundedConstraint(tp, lb, inTpl))
          case (list, List(ub)) if (list == Nil || list == List(NothingClass.tpe)) => // Only upper bound
            List(new UpperBoundedConstraint(tp, ub, inTpl))
          case (List(lb), List(ub)) =>                             // Single bounds, not obvious
            List(new BoundConstraint(tp, lb, ub, inTpl))
          case _ =>                                                // Multiple bounds
            List(new ComplexBoundsConstraint(tp, constr, inTpl))
        }
        if (DEBUG)
          debug("Type constraint reduction: " + constr + ": " + result)
        result
      }
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
}