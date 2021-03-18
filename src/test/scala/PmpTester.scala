package pmp

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import config.PmpConfig

import scala.sys.process._
import scala.util.Random

class PmpTester extends FunSuite {
  var compiled: SimCompiled[Pmp] = null
  val pmps = 16

  test("compile") {
    compiled = PmpConfig().compile(new Pmp(configs = pmps))
  }

  test("testbench") {
    compiled.doSim(seed = 1) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (idx <- 0 until (pmps / 4)) {
        dut.io.write #= true
        dut.io.select #= true
        dut.io.index #= idx
        dut.io.writeData #= BigInt("0f0f0f8f", 16)
        dut.clockDomain.waitSampling(1)
      }
      for (idx <- 0 until pmps) {
        dut.io.write #= true
        dut.io.select #= false
        dut.io.index #= idx
        dut.io.writeData #= BigInt("12345678", 16)
        dut.clockDomain.waitSampling(1)
      }
      for (idx <- 0 until pmps) {
        dut.io.write #= false
        dut.io.select #= false
        dut.io.index #= idx
        dut.clockDomain.waitSampling(2)
        assert(dut.io.readData.toBigInt == 0x12345678, 
          "dut.io.readData missmatch")
      }
    }
  }
}