package pmp

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import config.PmpConfig

import scala.sys.process._
import scala.util.Random

class PmpTester extends FunSuite {
  var compiled: SimCompiled[PmpController] = null
  val pmps = 16

  test("compile") {
    compiled = PmpConfig().compile(new PmpController(count = pmps))
  }

  test("testbench") {
    compiled.doSim(seed = 2) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (idx <- (pmps / 4 - 1) downto 0) {
        dut.io.write.valid #= true
        dut.io.select #= true
        dut.io.index #= idx << 2
        dut.io.write.payload #= BigInt("00880000", 16)
        dut.clockDomain.waitSampling(1)
      }
      for (idx <- (pmps / 4 - 1) downto 0) {
        dut.io.write.valid #= true
        dut.io.select #= true
        dut.io.index #= idx << 2
        dut.io.write.payload #= BigInt("000088ff", 16)
        dut.clockDomain.waitSampling(1)
      }
      for (idx <- 0 until pmps) {
        dut.io.write.valid #= true
        dut.io.select #= false
        dut.io.index #= idx
        dut.io.write.payload #= BigInt("12345678", 16)
        dut.clockDomain.waitSampling(1)
      }
      for (idx <- 0 until pmps) {
        dut.io.write.valid #= false
        dut.io.select #= false
        dut.io.index #= idx
        dut.clockDomain.waitSampling(1)
        assert(dut.io.read.toBigInt == 0x12345678, 
          "dut.io.readData missmatch")
      }
    }
  }
}