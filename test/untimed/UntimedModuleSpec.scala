// Copyright 2020, SiFive, Inc
// released under Apache License Version 2.0
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package untimed

import chisel3._
import org.scalatest._
import paso.UntimedModule
import paso.untimed.UntimedError


class UntimedInc extends UntimedModule {
  val inc = fun("inc").in(UInt(32.W)).out(UInt(32.W)) {
    (in, out) => out := in + 1.U
  }
}

class Counter4Bit extends UntimedModule {
  val value = RegInit(0.U(4.W))
  val inc = fun("inc").out(UInt(4.W)) { out =>
    value := value + 1.U
    out := value + 1.U
  }
}

class Counter4BitWithSubModule extends UntimedModule {
  val value = RegInit(0.U(4.W))
  val ii = UntimedModule(new UntimedInc)
  val inc = fun("inc").out(UInt(4.W)) { out =>
    val newValue = ii.inc(value)
    value := newValue
    out := newValue
  }
}

class RegInMethodModule extends UntimedModule {
  val thisIsOK = RegInit(0.U(3.W))

  val foo = fun("foo") {
    val thisIsNot = RegInit(0.U(3.W))
  }
}

class UntimedModuleSpec extends FlatSpec {
  "a simple UntimedModule" should "be elaborated with UntimedModule(new ...)" in {
    val m = UntimedModule(new UntimedInc)
    assert(m.isElaborated)
    assert(m.name == "UntimedInc")
    assert(m.methods.size == 1)
    val inc = m.methods.head
    assert(inc.name == "inc")

    // we should be able to access the low firrtl
    val f = m.getFirrtl
    assert(f.circuit.main == "UntimedInc")
    // as a crude check to see if the circuit is actually in LowForm, make sure there are no whens
    val src = f.circuit.serialize
    assert(!src.contains("when "))

    println(src)
  }

  "an UntimedModule with state" should "be elaborated with UntimedModule(new ...)" in {
    val m = UntimedModule(new Counter4Bit)
    assert(m.isElaborated)
    assert(m.name == "Counter4Bit")
    assert(m.methods.size == 1)
    assert(m.value.getWidth == 4)
    val inc = m.methods.head
    assert(inc.name == "inc")
  }

  "an UntimedModule with a sub module" should "be elaborated with UntimedModule(new ...)" in {
    val m = UntimedModule(new Counter4BitWithSubModule)
    assert(m.isElaborated)
    assert(m.name == "Counter4BitWithSubModule")
    assert(m.methods.size == 1)
    assert(m.value.getWidth == 4)
    val inc = m.methods.head
    assert(inc.name == "inc")
  }

  "declaring a register inside a method" should "lead to an exception" in {
    val err = intercept[UntimedError] {
      val m = UntimedModule(new RegInMethodModule)
    }
    assert(err.getMessage.contains("create a register"))
    assert(err.getMessage.contains("in method foo of RegInMethodModule"))
  }
}
