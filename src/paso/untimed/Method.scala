// Copyright 2020 The Regents of the University of California
// Copyright 2020, SiFive, Inc
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package paso.untimed

import chisel3._
import chisel3.experimental.{ChiselAnnotation, IO, annotate}
import firrtl.annotations.{Annotation, ModuleTarget, MultiTargetAnnotation, ReferenceTarget, SingleTargetAnnotation, Target}

import scala.collection.mutable

private[paso] trait MethodParent {
  private[paso] def addMethod(m: Method): Unit
  private[paso] def toTarget: ModuleTarget
  private[paso] def isElaborated: Boolean
}

trait Method {
  def name: String
  private[paso ]def guard: () => Bool
  private[paso] def generate(): Unit
}

case class NMethod(name: String, guard: () => Bool, impl: () => Unit, parent: MethodParent) extends Method {
  def apply(): Unit = {
    require(!parent.isElaborated, "TODO: implement method calls for elaborated UntimedMoudles")
    throw new NotImplementedError("Calling methods with side effects is currently not supported!")
  }
  override private[paso] def generate(): Unit = {
    val io = IO(new MethodIOBundle(UInt(0.W), UInt(0.W))).suggestName(name)
    io.guard := guard()
    when(io.enabled) { impl() }
  }
}

case class IMethod[I <: Data](name: String, guard: () => Bool, inputType: I, impl: I => Unit, parent: MethodParent) extends Method {
  def apply(in: I): Unit = {
    require(!parent.isElaborated, "TODO: implement method calls for elaborated UntimedMoudles")
    throw new NotImplementedError("Calling methods with side effects is currently not supported!")
  }
  override private[paso] def generate(): Unit = {
    val io = IO(new MethodIOBundle(inputType, UInt(0.W))).suggestName(name)
    io.guard := guard()
    when(io.enabled) { impl(io.arg) }
  }
}

case class OMethod[O <: Data](name: String, guard: () => Bool, outputType: O, impl: O => Unit, parent: MethodParent) extends Method {
  def apply(): O = {
    require(!parent.isElaborated, "TODO: implement method calls for elaborated UntimedMoudles")
    val ii = MethodCall.getCallCount(name)
    // create port to emulate the function call
    val call = IO(new OMethodCallBundle(outputType)).suggestName(name + "_call_" + ii)
    annotate(new ChiselAnnotation { override def toFirrtl: Annotation = MethodCallAnnotation(List(call.ret.toTarget), parent.toTarget, name, ii, false) })
    call.ret
  }
  override private[paso] def generate(): Unit = {
    val io = IO(new MethodIOBundle(UInt(0.W), outputType)).suggestName(name)
    io.guard := guard()
    io.ret := DontCare
    when(io.enabled) { impl(io.ret) }
  }
}

case class IOMethod[I <: Data, O <: Data](name: String, guard: () => Bool, inputType: I, outputType: O, impl: (I,O) => Unit, parent: MethodParent) extends Method {
  def apply(in: I): O = {
    require(!parent.isElaborated, "TODO: implement method calls for elaborated UntimedMoudles")
    val ii = MethodCall.getCallCount(name)
    // create port to emulate the function call
    val call = IO(new IOMethodCallBundle(inputType, outputType)).suggestName(name + "_call_" + ii)
    annotate(new ChiselAnnotation { override def toFirrtl: Annotation = MethodCallAnnotation(List(call.arg.toTarget), parent.toTarget, name, ii, true) })
    annotate(new ChiselAnnotation { override def toFirrtl: Annotation = MethodCallAnnotation(List(call.ret.toTarget), parent.toTarget, name, ii, false) })
    call.arg := in
    call.ret
  }
  override private[paso] def generate(): Unit = {
    val io = IO(new MethodIOBundle(inputType, outputType)).suggestName(name)
    io.guard := guard()
    io.ret := DontCare
    when(io.enabled) { impl(io.arg, io.ret) }
  }
}


class OMethodCallBundle[O <: Data](outputType: O) extends Bundle {
  val ret = Input(outputType)
  override def cloneType: this.type = {
    new OMethodCallBundle(outputType).asInstanceOf[this.type]
  }
}
class IOMethodCallBundle[I <: Data, O <: Data](inputType: I, outputType: O) extends Bundle {
  val arg = Output(inputType)
  val ret = Input(outputType)
  override def cloneType: this.type = {
    new IOMethodCallBundle(inputType, outputType).asInstanceOf[this.type]
  }
}
class MethodIOBundle[I <: Data, O <: Data](inputType: I, outputType: O) extends Bundle {
  val enabled = Input(Bool())
  val guard = Output(Bool())
  val arg = Input(inputType)
  val ret = Output(outputType)
  override def cloneType: this.type = {
    new MethodIOBundle(inputType, outputType).asInstanceOf[this.type]
  }
}

// singleton to ensure that all call sites get unique names (this is a bit ugly and not thread-safe :( )
object MethodCall {
  private val callSiteCount = mutable.HashMap[String, Int]()
  def getCallCount(name: String): Int = {
    val old = callSiteCount.getOrElse(name, -1)
    val next = old + 1
    callSiteCount(name) = next
    next
  }
}

case class MethodCallAnnotation(signals: Seq[ReferenceTarget], parent: ModuleTarget, name: String, ii: Int, isArg: Boolean) extends MultiTargetAnnotation {
  override val targets = List(signals, List(parent))

  override def duplicate(n: Seq[Seq[Target]]) = {
    assert(n.length == 2, "Need signal + parent")
    assert(n(1).length == 1, "Method parent should always stay a single ModuleTarget!")
    copy(signals = n.head.map(_.asInstanceOf[ReferenceTarget]), parent = n(1).head.asInstanceOf[ModuleTarget])
  }
}