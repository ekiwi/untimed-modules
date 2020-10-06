// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package paso.untimed

import firrtl.{AnnotationSeq, CircuitState, Namespace, ir}
import firrtl.Utils.getUIntWidth
import firrtl.analyses.InstanceKeyGraph
import firrtl.analyses.InstanceKeyGraph.InstanceKey
import firrtl.ir.IntWidth
import firrtl.passes.PassException
import paso.{ChiselCompiler, FirrtlCompiler, UntimedModule}

import scala.collection.mutable





object UntimedCompiler {

}

case class MethodInfo(name: String, parent: String, ioName: String, writes: Set[String], calls: Seq[CallInfo])
case class CallInfo(parent: String, method: String, ioName: String)
case class UntimedModuleInfo(name: String, state: Seq[ir.Reference], methods: Seq[MethodInfo], submodules: Seq[UntimedModuleInfo]) {
  val hasState: Boolean = state.nonEmpty || (submodules.count(_.hasState) > 0)
}




/** Runs on the high firrtl representation of the untimed module circuit.
 *  Collects all calls to submodules, integrates them into guards, initializes method call wires
 *  properly and creates the call graph.
 * */
object CollectCalls {

  def run(state: CircuitState, abstracted: Set[String]): CircuitState = {
    assert(abstracted.isEmpty, "TODO: allow submodules to be abstracted!")
    val (newModules, mainInfo) = run(state.circuit.main, state, abstracted)
    val annos = state.annotations.filterNot(a => a.isInstanceOf[MethodIOAnnotation] || a.isInstanceOf[MethodCallAnnotation])
    state.copy(circuit = state.circuit.copy(modules = newModules), annotations = annos)
  }

  private def run(name: String, state: CircuitState, abstracted: Set[String]): (Seq[ir.Module], UntimedModuleInfo) = {
    val mod = state.circuit.modules.collectFirst{ case m: ir.Module if m.name == name => m }.get
    val calls = state.annotations.collect { case  a: MethodCallAnnotation if a.callIO.module == mod.name => a}

    // analyze submodules first
    val instances = InstanceKeyGraph.collectInstances(mod)
    val submods = instances.map(s => run(s.module, state, abstracted))

    // collect metadata: state (regs + mems) + analyze methods
    val localState = findState(mod)
    val methods = analyzeMethods(mod, calls, state.annotations)
    val info = UntimedModuleInfo(name, localState, methods, submods.map(_._2))

    // verify that all method calls are correct
    val nameToModule = info.submodules.map(s => s.name -> s).toMap
    verifyMethodCalls(name, methods, nameToModule)

    // calculate the number of instances for each submodule
    val namespace = firrtl.Namespace(mod)
    val finalInstances = info.submodules.flatMap { s =>
      val instanceName = instances.find(_.module == s.name).map(_.name).get
      // stateful modules only ever have a single instance since we need one "true" copy of the state
      if(s.hasState) {
        Some(s.name -> List(instanceName))
      } else {
        // find the max number of calls to any method of this module in a single method
        val maxCalls = methods.flatMap(m => m.calls.filter(_.parent == s.name).groupBy(_.method).map(_._2.size)).max
        // if this module is never called
        if(maxCalls == 0) { None } else {
          val copies = (0 until (maxCalls - 1)).map(_ => namespace.newName(instanceName))
          Some(s.name -> (List(instanceName) ++ copies))
        }
      }
    }.toMap

    // statements to declare instances and connect their reset and clock and inputs
    val instanceStatements = finalInstances.flatMap{ case (module, is) => is.flatMap(i => makeInstance(i, module)) }

    // connect calls to instances
    val connectCalls = methods.flatMap { meth =>
      val callCount = mutable.HashMap[String, Int]()
      meth.calls.map { call =>
        val fullName = s"${call.parent}.${call.method}"
        val count = callCount.getOrElseUpdate(fullName, 0)
        callCount(fullName) += 1
        val instance = finalInstances(call.parent)(count)
        // connect to instance
        val info = mod.ports.find(_.name == call.ioName).get.info
        val stmts = connectCallToInstanceAndDefaults(call, instance, info)
        ((call.ioName -> instance), stmts)
      }
    }
    val callToInstance = connectCalls.map(_._1)
    val connectCallStmts = connectCalls.flatMap(_._2)



    // TODO: create instances and connect to calls

    // provide default connections to call IO
    val defaults: Seq[ir.Statement] = calls.flatMap { c =>
      val info = mod.ports.find(_.name == c.callIO.ref).get.info
      // by default a call is not enabled
      val en = ir.Connect(info, ir.SubField(ir.Reference(c.callIO.ref), "enabled"), ir.UIntLiteral(0, IntWidth(1)))
      // by default the arguments to the call are undefined
      val arg = ir.IsInvalid(info, ir.SubField(ir.Reference(c.callIO.ref), "arg"))
      List(en, arg)
    }

    val body = ir.Block(defaults :+ mod.body)
    val newMod = mod.copy(body=body)

    (submods.flatMap(_._1) :+ newMod, info)
  }

  def connectCallToInstanceAndDefaults(call: CallInfo, instanceName: String, info: ir.Info = ir.NoInfo): List[ir.Statement] = List(
    // by default the call is disabled and the arg is DontCare
    ir.Connect(info, ir.SubField(ir.Reference(call.ioName), "enabled"), ir.UIntLiteral(0, IntWidth(1))),
    ir.IsInvalid(info, ir.SubField(ir.Reference(call.ioName), "arg")),
    // connect method ret to call io
    ir.Connect(info, ir.SubField(ir.Reference(call.ioName), "ret"), ir.SubField(ir.SubField(ir.Reference(instanceName), call.method), "ret")),
  )


  def makeInstance(name: String, module: String, info: ir.Info = ir.NoInfo): List[ir.Statement] = List(
    ir.DefInstance(info, name, module),
    ir.Connect(info, ir.SubField(ir.Reference(name), "reset"), ir.Reference("reset")),
    ir.Connect(info, ir.SubField(ir.Reference(name), "clock"), ir.Reference("clock")),
    // by default a method is disabled (this is important in case it is never called!)
    ir.Connect(info, ir.SubField(ir.Reference(name), "enabled"), ir.UIntLiteral(0, IntWidth(1))),
    // by default a method input is don't care
    ir.IsInvalid(info, ir.SubField(ir.Reference(name), "arg")),
  )

  def verifyMethodCalls(name: String, methods: Iterable[MethodInfo], nameToModule: Map[String, UntimedModuleInfo]): Unit = {
    methods.foreach { m =>
      val callsByParent = m.calls.groupBy(_.parent)
      callsByParent.foreach { case (parent, calls) =>
        assert(nameToModule.contains(parent), s"$parent is not a submodule of $name!")
        val parentMod = nameToModule(parent)
        if(parentMod.hasState) {
          // if the submodule is stateful, we can only call one method
          if(calls.size > 1) {
            val cs = "Detected calls: " + calls.map(_.method).mkString(", ")
            throw new UntimedError(s"[$name.${m.name}] cannot call more than one method of stateful submodule $parent.\n$cs")
          }
        } // if the submodule is not stateful, we can call as many methods as we want to!
      }
    }
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
    val callIO = calls.map(c => c.callIO.ref -> CallInfo(c.calleeParent.module, c.calleeName, c.callIO.ref)).toMap

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
          methods.append(analyzeMethod(ioToName(ioName), mod.name, ioName, c.conseq, callIO))
        case _ => // methods should not be enclosed in other when blocks!
      }
      // TODO: check for state updates outside of methods
      //       we can either completely disallow them or treat them as some sort of init block
      case other => other.foreachStmt(onStmt)
    }
    mod.foreachStmt(onStmt)

    methods.toSeq
  }

  /**
   * We need to extract some information about the method:
   * - what state it writes to
   * - what calls it makes
   */
  def analyzeMethod(name: String, parent: String, ioName: String, body: ir.Statement, calls: Map[String, CallInfo]): MethodInfo = {
    val locals = mutable.HashSet[String]()
    val writes = mutable.HashSet[String]()
    val localCalls = mutable.HashSet[String]()

    def onWrite(loc: ir.Expression): Unit = {
      val signal = loc.serialize.split('.').head
      if(!locals.contains(signal) && signal != ioName && !calls.contains(signal)) {
        writes.add(signal)
      }
    }

    def onStmt(s: ir.Statement): Unit = s match {
      case ir.DefNode(_, name, _) => locals.add(name)
      case ir.DefWire(_, name, _) => locals.add(name)
      case r : ir.DefRegister =>
        throw new UntimedError(s"Cannot create a register ${r.name} in method $name of $parent!")
      case m : ir.DefMemory =>
        throw new UntimedError(s"Cannot create a memory ${m.name} in method $name of $parent!")
      case i : ir.DefInstance =>
        throw new UntimedError(s"Cannot create an instance ${i.name} of ${i.module} in method $name of $parent!")
      case ir.Connect(_, loc, _) =>
        loc match {
          case ir.SubField(ir.Reference(maybeCall, _, _, _), "enabled", _, _) if calls.contains(maybeCall) =>
            localCalls.add(maybeCall)
          case _ => onWrite(loc)
        }
      case ir.IsInvalid(_, loc) => onWrite(loc)
      case other => other.foreachStmt(onStmt)
    }
    onStmt(body)

    MethodInfo(name, parent, ioName, writes.toSet, localCalls.toSeq.map(calls))
  }



}

class UntimedError(s: String) extends PassException(s)