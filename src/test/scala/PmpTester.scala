package pmp

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import config.PmpConfig

import scala.sys.process._
import scala.util.Random

class PmpTester extends FunSuite {
  var compiled: SimCompiled[Pmp] = null

  test("compile") {
    compiled = PmpConfig().compile(new Pmp(configs = 4))
  }

  test("testbench") {
    compiled.doSim(seed = 42) { dut =>
      dut.clockDomain.forkStimulus(10)
      var pmp = 0
      for(_ <- 0 until 100) {
        dut.io.write #= true
      }
    }
  }
}