// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package paso.untimed

import firrtl.{AnnotationSeq, CircuitState, ir}
import firrtl.Utils.getUIntWidth
import firrtl.analyses.InstanceKeyGraph
import firrtl.analyses.InstanceKeyGraph.InstanceKey
import firrtl.ir.IntWidth
import paso.{ChiselCompiler, FirrtlCompiler, UntimedModule}

import scala.collection.mutable





object UntimedCompiler {

}

case class MethodInfo(name: String, ioName: String, reads: List[String], writes: List[String], calls: List[CallInfo])
case class CallInfo(parent: String, method: String, ioName: String)
case class UntimedModuleInto(name: String, state: List[String], methods: List[String])




/** Runs on the high firrtl representation of the untimed module circuit.
 *  Collects all calls to submodules, integrates them into guards, initializes method call wires
 *  properly and creates the call graph.
 * */
object CollectCalls {

  def run(state: CircuitState, abstracted: Set[String]): CircuitState = {
    assert(abstracted.isEmpty, "TODO: allow submodules to be abstracted!")
    val newMain = run(state.circuit.main, state, abstracted)
    val annos = state.annotations.filterNot(a => a.isInstanceOf[MethodIOAnnotation] || a.isInstanceOf[MethodCallAnnotation])
    state.copy(circuit = state.circuit.copy(modules = List(newMain)), annotations = annos)
  }

  private def run(name: String, state: CircuitState, abstracted: Set[String]): ir.Module = {
    val mod = state.circuit.modules.collectFirst{ case m: ir.Module if m.name == name => m }.get
    val calls = state.annotations.collect { case  a: MethodCallAnnotation if a.callIO.module == mod.name => a}

    // analyze submodules first
    val submods = InstanceKeyGraph.collectInstances(mod).map(s => run(s.module, state, abstracted))

    // collect state
    val localState = findState(mod)

    // analyze methods
    val methods = analyzeMethods(mod, calls, state.annotations)



    // collect the names of all instances and remove the instance definition form the body
    // TODO: we might not actually want to remove instances, if we are inlining!
    //val (mod, submods) = removeInstances(m)

    // provide default connections to call IO
    val defaults: Seq[ir.Statement] = calls.flatMap { c =>
      val info = mod.ports.find(_.name == c.callIO.ref).get.info
      // by default a call is not enabled
      val en = ir.Connect(info, ir.SubField(ir.Reference(c.callIO.ref), "enabled"), ir.UIntLiteral(0, IntWidth(1)))
      // by default the arguments to the call are undefined
      val arg = ir.IsInvalid(info, ir.SubField(ir.Reference(c.callIO.ref), "arg"))
      List(en, arg)
    }

    println(s"${calls.length} method calls in ${mod.name}")

    val body = ir.Block(defaults :+ mod.body)
    mod.copy(body=body)
  }

  def removeInstances(m: ir.Module): (ir.Module, Seq[InstanceKey]) = {
    val instances = mutable.ArrayBuffer[InstanceKey]()

    def onStmt(s: ir.Statement): ir.Statement = s match {
      case ir.DefInstance(_, name, module, _) =>
        // remove the instance definition
        instances += InstanceKey(name, module) ; ir.EmptyStmt
      case _: firrtl.WDefInstanceConnector =>
        firrtl.Utils.throwInternalError("Expecting WDefInstance, found a WDefInstanceConnector!")
      case c @ ir.Connect(_, loc, _) =>
        // remove the connections to the instance clock and reset
        val locString = loc.serialize
        val toInstance = instances.exists(i => locString.startsWith(i.name + "."))
        if(toInstance) ir.EmptyStmt else c
      case other => other.mapStmt(onStmt)
    }

    val newM = m.copy(body=onStmt(m.body))
    (newM, instances.toSeq)
  }


  def findState(m: ir.Module): Seq[ir.Reference] = {
    val state = mutable.ArrayBuffer[ir.Reference]()

    def onStmt(s: ir.Statement): Unit = s match {
      case r : ir.DefRegister => state.append(ir.Reference(r.name, r.tpe))
      case m : ir.DefMemory =>
        val arrayTpe = ir.VectorType(m.dataType, m.depth.toInt)
        state.append(ir.Reference(m.name, arrayTpe))
      case other => other.foreachStmt(onStmt)
    }
    m.foreachStmt(onStmt)

    state.toSeq
  }

  def analyzeMethods(mod: ir.Module, calls: Iterable[MethodCallAnnotation], annos: AnnotationSeq): Seq[MethodInfo] = {
    // method are of the following general pattern:
    // ```
    // method.guard <= UInt<1>("h1")
    // method.ret is invalid
    // when method.enabled :
    //   ; method body
    // ```
    // we know all the method names, so we can just search for a Conditionally that checks the `method_name.enabled` signal
    val ioToName = annos.collect{ case a: MethodIOAnnotation if a.target.module == mod.name => a.target.ref -> a.name }.toMap

    val methods = mutable.ArrayBuffer[MethodInfo]()

    def onStmt(s: ir.Statement): Unit = s match {
      case c : ir.Conditionally => c.pred match {
        case ir.SubField(ir.Reference(ioName, _, _, _), "enabled", _, _) if ioToName.contains(ioName) =>
          assert(c.alt == ir.EmptyStmt)
          methods.append(analyzeMethod(ioToName(ioName), ioName, c.conseq, calls))
        case _ => // methods should not be enclosed in other when blocks!
      }
      case other => other.foreachStmt(onStmt)
    }
    mod.foreachStmt(onStmt)

    methods.toSeq
  }

  /**
   * Finds signals read and connected to that are defined outside of the method as well as any calls inside the method.
   */
  def analyzeMethod(name: String, ioName: String, body: ir.Statement, calls: Iterable[MethodCallAnnotation]): MethodInfo = {
    println("TODO")

    MethodInfo(name, ioName, List(), List(), List())
  }


}