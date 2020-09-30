// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package paso.untimed

import firrtl.CircuitState
import firrtl.ir
import firrtl.analyses.InstanceKeyGraph
import paso.{ChiselCompiler, FirrtlCompiler, UntimedModule}





object UntimedCompiler {

}


/** Runs on the high firrtl representation of the untimed module circuit.
 *  Collects all calls to submodules, integrates them into guards, initializes method call wires
 *  properly and creates the call graph.
 * */
object CollectCalls {

  def run(state: CircuitState, abstracted: Set[String]): CircuitState = {
    assert(abstracted.isEmpty, "TODO: allow submodules to be abstracted!")

    val main = state.circuit.modules.collectFirst{ case m: ir.Module if m.name == state.circuit.main => m }.get
    val newMain = run(main, state, abstracted)

    state.copy(circuit = state.circuit.copy(modules = List(newMain)))
  }

  private def run(mod: ir.Module, state: CircuitState, abstracted: Set[String]): ir.Module = {
    val calls = state.annotations.collect { case  a: MethodCallAnnotation if a.signals.head.module == mod.name => a}
    val submods = InstanceKeyGraph.collectInstances(mod)

    println(s"${calls.length} method calls in ${mod.name}")

    mod
  }


}