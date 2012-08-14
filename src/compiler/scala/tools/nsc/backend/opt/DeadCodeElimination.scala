/* NSC -- new scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Iulian Dragos
 */


package scala.tools.nsc
package backend.opt

import scala.collection.{ mutable, immutable }
import symtab._

/**
 */
abstract class DeadCodeElimination extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions.RuntimePackage

  val phaseName = "dce"

  /** Create a new phase */
  override def newPhase(p: Phase) = new DeadCodeEliminationPhase(p)

  /** Dead code elimination phase.
   */
  class DeadCodeEliminationPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName
    val dce = new DeadCode()

    override def apply(c: IClass) {
      if (settings.Xdce.value)
        dce.analyzeClass(c)
    }
  }

  /** closures that are instantiated at least once, after dead code elimination */
  val liveClosures: mutable.Set[Symbol] = new mutable.HashSet()

  /** Remove dead code.
   */
  class DeadCode {

    def analyzeClass(cls: IClass) {
      cls.methods.foreach { m =>
        this.method = m
        dieCodeDie(m)
        global.closureElimination.peephole(m)
      }
    }

    val rdef = new reachingDefinitions.ReachingDefinitionsAnalysis;

    /** Use-def chain: give the reaching definitions at the beginning of given instruction. */
    var defs: immutable.Map[(BasicBlock, Int), immutable.Set[rdef.lattice.Definition]] = immutable.HashMap.empty

    /** Useful instructions which have not been scanned yet. */
    val worklist: mutable.Set[(BasicBlock, Int)] = new mutable.LinkedHashSet

    /** what instructions have been marked as useful? */
    val useful: mutable.Map[BasicBlock, mutable.BitSet] = perRunCaches.newMap()

    /** what local variables have been accessed at least once? */
    var accessedLocals: List[Local] = Nil

    /** the current method. */
    var method: IMethod = _

    /** Map instructions who have a drop on some control path, to that DROP instruction. */
    val dropOf: mutable.Map[(BasicBlock, Int), List[(BasicBlock, Int)]] = perRunCaches.newMap()

    private val tfa: analysis.MethodTFA = new analysis.MethodTFA()
    private var tfaCache: Map[Int, tfa.lattice.Elem] = Map.empty
    private var analyzedMethod: IMethod = NoIMethod

    def dieCodeDie(m: IMethod) {
      if (m.hasCode) {
        analyzedMethod = NoIMethod
        tfaCache = Map.empty
        log("dead code elimination on " + m);
        dropOf.clear()
        m.code.blocks.clear()
        accessedLocals = m.params.reverse
        m.code.blocks ++= linearizer.linearize(m)
        collectRDef(m)
        mark
        printMethod(m)
        sweep(m)
        accessedLocals = accessedLocals.distinct
        if ((m.locals diff accessedLocals).nonEmpty) {
          log("Removed dead locals: " + (m.locals diff accessedLocals))
          m.locals = accessedLocals.reverse
        }
      }
    }

    /** collect reaching definitions and initial useful instructions for this method. */
    def collectRDef(m: IMethod): Unit = if (m.hasCode) {
      defs = immutable.HashMap.empty; worklist.clear(); useful.clear();
      rdef.init(m);
      rdef.run;

      m foreachBlock { bb =>
        useful(bb) = new mutable.BitSet(bb.size)
        var rd = rdef.in(bb);
        for (Pair(i, idx) <- bb.toList.zipWithIndex) {
          i match {

            case LOAD_LOCAL(l) =>
              defs = defs + Pair(((bb, idx)), rd.vars)

            case STORE_LOCAL(_) =>
              /* SI-4935 Check whether a module is stack top, if so mark the instruction that loaded it
               * (otherwise any side-effects of the module's constructor go lost).
               *   (a) The other two cases where a module's value is stored (STORE_FIELD and STORE_ARRAY_ITEM)
               *       are already marked (case clause below).
               *   (b) A CALL_METHOD targeting a method `m1` where the receiver is potentially a module (case clause below)
               *       will have the module's load marked provided `isSideEffecting(m1)`.
               *       TODO check for purity (the ICode?) of the module's constructor (besides m1's purity).
               *       See also https://github.com/paulp/scala/blob/topic/purity-analysis/src/compiler/scala/tools/nsc/backend/opt/DeadCodeElimination.scala
               */
              val necessary = rdef.findDefs(bb, idx, 1) exists { p =>
                val (bb1, idx1) = p
                bb1(idx1) match {
                  case LOAD_MODULE(module) => isLoadNeeded(module)
                  case _                   => false
                }
              }
              if (necessary) worklist += ((bb, idx))

            case RETURN(_) | JUMP(_) | CJUMP(_, _, _, _) | CZJUMP(_, _, _, _) | STORE_FIELD(_, _) |
                 THROW(_)   | LOAD_ARRAY_ITEM(_) | STORE_ARRAY_ITEM(_) | SCOPE_ENTER(_) | SCOPE_EXIT(_) | STORE_THIS(_) |
                 LOAD_EXCEPTION(_) | SWITCH(_, _) | MONITOR_ENTER() | MONITOR_EXIT() => worklist += ((bb, idx))
            case CALL_METHOD(m1, _) if isSideEffecting(m1) => worklist += ((bb, idx)); log("marking " + m1)
            case CALL_METHOD(m1, SuperCall(_)) =>
              worklist += ((bb, idx)) // super calls to constructor
            case DROP(_) =>
              val necessary = rdef.findDefs(bb, idx, 1) exists { p =>
                val (bb1, idx1) = p
                bb1(idx1) match {
                  case CALL_METHOD(m1, _) if isSideEffecting(m1) => true
                  case LOAD_EXCEPTION(_) | DUP(_) | LOAD_MODULE(_) => true
                  case _ =>
                    dropOf((bb1, idx1)) = (bb,idx) :: dropOf.getOrElse((bb1, idx1), Nil)
//                    println("DROP is innessential: " + i + " because of: " + bb1(idx1) + " at " + bb1 + ":" + idx1)
                    false
                }
              }
              if (necessary) worklist += ((bb, idx))
            case _ => ()
          }
          rd = rdef.interpret(bb, idx, rd)
        }
      }
    }

    private def isLoadNeeded(module: Symbol): Boolean = {
      module.info.member(nme.CONSTRUCTOR).filter(isSideEffecting) != NoSymbol
    }

    /** Mark useful instructions. Instructions in the worklist are each inspected and their
     *  dependencies are marked useful too, and added to the worklist.
     */
    def mark() {
//      log("Starting with worklist: " + worklist)
      while (!worklist.isEmpty) {
        val (bb, idx) = worklist.iterator.next
        worklist -= ((bb, idx))
        debuglog("Marking instr: \tBB_" + bb + ": " + idx + " " + bb(idx))

        val instr = bb(idx)
        if (!useful(bb)(idx)) {
          useful(bb) += idx
          dropOf.get(bb, idx) foreach {
              for ((bb1, idx1) <- _)
                useful(bb1) += idx1
          }
          instr match {
            case LOAD_LOCAL(l1) =>
              for ((l2, bb1, idx1) <- defs((bb, idx)) if l1 == l2; if !useful(bb1)(idx1)) {
                log("\tAdding " + bb1(idx1))
                worklist += ((bb1, idx1))
              }

            case nw @ NEW(REFERENCE(sym)) =>
              assert(nw.init ne null, "null new.init at: " + bb + ": " + idx + "(" + instr + ")")
              worklist += findInstruction(bb, nw.init)
              if (inliner.isClosureClass(sym)) {
                liveClosures += sym
              }

            // it may be better to move static initializers from closures to
            // the enclosing class, to allow the optimizer to remove more closures.
            // right now, the only static fields in closures are created when caching
            // 'symbol literals.
            case LOAD_FIELD(sym, true) if inliner.isClosureClass(sym.owner) =>
              log("added closure class for field " + sym)
              liveClosures += sym.owner

            case LOAD_EXCEPTION(_) =>
              ()

            case _ =>
              for ((bb1, idx1) <- rdef.findDefs(bb, idx, instr.consumed) if !useful(bb1)(idx1)) {
                log("\tAdding " + bb1(idx1))
                worklist += ((bb1, idx1))
              }
          }
        }
      }
    }

    def sweep(m: IMethod) {
      val compensations = computeCompensations(m)

      m foreachBlock { bb =>
        val oldInstr = bb.toList
        bb.open
        bb.clear
        for (Pair(i, idx) <- oldInstr.zipWithIndex) {
          if (useful(bb)(idx)) {
            bb.emit(i, i.pos)
            compensations.get(bb, idx) match {
              case Some(is) => is foreach bb.emit
              case None => ()
            }
            // check for accessed locals
            i match {
              case LOAD_LOCAL(l) if !l.arg =>
                accessedLocals = l :: accessedLocals
              case STORE_LOCAL(l) if !l.arg =>
                accessedLocals = l :: accessedLocals
              case _ => ()
            }
          } else {
            i match {
              case NEW(REFERENCE(sym)) =>
                log("skipped object creation: " + sym + "inside " + m)
              case _ => ()
            }
            debuglog("Skipped: bb_" + bb + ": " + idx + "( " + i + ")")
          }
        }

        if (bb.nonEmpty) bb.close
        else log("empty block encountered")
      }
    }

    private def computeCompensations(m: IMethod): mutable.Map[(BasicBlock, Int), List[Instruction]] = {
      val compensations: mutable.Map[(BasicBlock, Int), List[Instruction]] = new mutable.HashMap

      m foreachBlock { bb =>
        assert(bb.closed, "Open block in computeCompensations")
        foreachWithIndex(bb.toList) { (i, idx) =>
          if (!useful(bb)(idx)) {
            log("!useful: " + i)
            foreachWithIndex(i.consumedTypes.reverse) { (consumedType, depth) =>
              log("Finding definitions of: " + i + "\n\t" + consumedType + " at depth: " + depth)
              val defs = rdef.findDefs(bb, idx, 1, depth)
              for (d <- defs) {
                val (bb, idx) = d
                bb(idx) match {
                  case DUP(_) if idx > 0 =>
                    bb(idx - 1) match {
                      case nw @ NEW(_) =>
                        val init = findInstruction(bb, nw.init)
                        log("Moving DROP to after <init> call: " + nw.init)
                        compensations(init) = List(DROP(consumedType))
                      case _ =>
                        compensations(d) = List(DROP(consumedType))
                    }
                  case _ =>
                    compensations(d) = List(DROP(consumedType))
                }
              }
            }
          }
        }
      }
      compensations
    }

    private def withClosed[a](bb: BasicBlock)(f: => a): a = {
      if (bb.nonEmpty) bb.close
      val res = f
      if (bb.nonEmpty) bb.open
      res
    }

    private def findInstruction(bb: BasicBlock, i: Instruction): (BasicBlock, Int) = {
      for (b <- linearizer.linearizeAt(method, bb)) {
        val idx = b.toList indexWhere (_ eq i)
        if (idx != -1)
          return (b, idx)
      }
      abort("could not find init in: " + method)
    }

    private def isPure(sym: Symbol) = (
         (sym.isGetter && sym.isEffectivelyFinal && !sym.isLazy)
      || (sym.isPrimaryConstructor && (sym.enclosingPackage == RuntimePackage || inliner.isClosureClass(sym.owner)))
    )
    /** Is 'sym' a side-effecting method? TODO: proper analysis.  */
    private def isSideEffecting(sym: Symbol) = !isPure(sym)

    private var margin = 0
    private var out = new java.io.PrintWriter(System.out, true)

    final val TAB = 2


    def indent() { margin += TAB }
    def undent() { margin -= TAB }

    def print(s: String) { out.print(s) }
    def print(o: Any) { print(o.toString()) }

    def println(s: String) {
      print(s);
      println
    }

    def println() {
      out.println()
      var i = 0
      while (i < margin) {
        print(" ");
        i += 1
      }
    }

    def printList[A](l: List[A], sep: String): Unit = l match {
      case Nil =>
      case x :: Nil => print(x)
      case x :: xs  => print(x); print(sep); printList(xs, sep)
    }

    def printList[A](pr: A => Unit)(l: List[A], sep: String): Unit = l match {
      case Nil =>
      case x :: Nil => pr(x)
      case x :: xs  => pr(x); print(sep); printList(pr)(xs, sep)
    }

    def printClass(cls: IClass) {
      print(cls.symbol.toString()); print(" extends ");
      printList(cls.symbol.info.parents, ", ");
      indent; println(" {");
      println("// fields:");
      cls.fields.foreach(printField); println;
      println("// methods");
      cls.methods.foreach(printMethod);
      undent; println;
      println("}")
    }

    def printField(f: IField) {
      print(f.symbol.keyString); print(" ");
      print(f.symbol.nameString); print(": ");
      println(f.symbol.info.toString());
    }

    def printMethod(m: IMethod) {
      print("def "); print(m.symbol.name);
      print("("); printList(printParam)(m.params, ", "); print(")");
      print(": "); print(m.symbol.info.resultType)

      if (!m.isAbstractMethod) {
        println(" {")
        println("locals: " + m.locals.mkString("", ", ", ""))
        println("startBlock: " + m.startBlock)
        println("blocks: " + m.code.blocks.mkString("[", ",", "]"))
        println
        linearizer.linearize(m) foreach printBlock
        println("}")

        indent; println("Exception handlers: ")
        m.exh foreach printExceptionHandler

        undent; println
      } else
        println
    }

    def printParam(p: Local) {
      print(p.sym.name); print(": "); print(p.sym.info);
      print(" ("); print(p.kind); print(")")
    }

    def printExceptionHandler(e: ExceptionHandler) {
      indent;
      println("catch (" + e.cls.simpleName + ") in " + e.covered.toSeq.sortBy(_.label) + " starting at: " + e.startBlock);
      println("consisting of blocks: " + e.blocks);
      undent;
      println("with finalizer: " + e.finalizer);
    }

    def printBlock(bb: BasicBlock) {
      print(bb.label)
      if (bb.loopHeader) print("[loop header]")
      print(": ");
      print("pred: " + bb.predecessors + " succs: " + bb.successors + " flags: " + bb.flagsString)
      indent; println
      bb.toList foreach ((i: Instruction) => printInstruction(bb, i))
      undent; println
    }

    def printInstruction(bb: BasicBlock, i: Instruction) {
      if (i.pos.isDefined) print(i.pos.line.toString + "\t") else print("?\t")
      println("%-60s".format(i.toString()) + "  " + (if (useful(bb)(bb.indexOf(i))) "            " else "***remove***") + "        " + getTypesAtInstruction(bb, bb.indexOf(i)).reverse)
    }

    private def getTypesAtInstruction(bblock: BasicBlock, index: Int): List[TypeKind] = {
      // get the stack at the block entry
      var typeInfo = getTypesAtBlockEntry(bblock)

      // perform tfa to the current instruction
      for (i <- 0 to (index)) {
        typeInfo = tfa.interpret(typeInfo, bblock(i))
      }

      // return the result
      typeInfo.stack.types
    }

    /**
      * Gets the stack at the block entry. Normally the typeFlowAnalysis should be run again, but we know how to compute
      * the stack for handler duplicates. For the locals, it's safe to assume the info from the original handler is
      * still valid (a more precise analysis can be done, but it's not necessary)
      */
    private def getTypesAtBlockEntry(bblock: BasicBlock): tfa.lattice.Elem = {
      // lazily perform tfa, because it's expensive
      // cache results by block label, as rewriting the code messes up the block's hashCode
      if (analyzedMethod != bblock.method) {
        analyzedMethod = bblock.method
        tfa.init(bblock.method)
        tfa.run

        for (block <- bblock.method.blocks.sortBy(_.label))
          tfaCache += block.label -> tfa.in(block)
      }
      tfaCache(bblock.label)
    }
  } /* DeadCode */
}
