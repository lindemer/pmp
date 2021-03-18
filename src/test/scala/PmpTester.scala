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
    compiled = PmpConfig().compile(new Pmp(configs = 16))
  }

  test("testbench") {
    compiled.doSim(seed = 42) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (idx <- 0 until 16) {
        dut.io.write #= true
        dut.io.select #= false
        dut.io.index #= idx
        dut.io.writeData #= BigInt("0", 16)
        dut.clockDomain.waitSampling(Random.nextInt(10))
      }
      for (idx <- 0 until 16) {
        dut.io.write #= false
        dut.io.select #= false
        dut.io.index #= idx
        dut.clockDomain.waitSampling(Random.nextInt(10))
        assert(dut.io.readData == 0xffffffff, "dut.io.readData missmatch")
      }
    }
  }
}