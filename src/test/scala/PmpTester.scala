/*
 * Copyright (c) 2021 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package pmp

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import config.PmpConfig

import scala.sys.process._
import scala.util.Random

class PmpTester extends FunSuite {
  var compiled: SimCompiled[PmpController] = null
  val count = 16

  var vec0 = Array.fill(count){Random.nextInt(1000000000)}
  var vec1 = Array.fill(count){Random.nextInt(1000000000)}

  test("compile") {
    compiled = PmpConfig().compile(new PmpController(count = count))
  }

  test("locking") {
    compiled.doSim(seed = 2) { dut =>
      dut.clockDomain.forkStimulus(10)

      // Write the pmpaddr# registers.
      for (idx <- 0 until count) {
        dut.io.write.valid #= true
        dut.io.config #= false
        dut.io.index #= idx
        dut.io.write.payload #= vec0(idx)
        dut.clockDomain.waitSampling(1)
        while (!dut.io.write.ready.toBoolean) {
          dut.clockDomain.waitSampling(1)
        }
      }

      // Write the pmpcfg# registers.
      for (idx <- Range(0, count, 4)) {
        dut.io.write.valid #= true
        dut.io.config #= true
        dut.io.index #= idx
        dut.io.write.payload #= BigInt("07880789", 16)
        dut.clockDomain.waitSampling(1)
        while (!dut.io.write.ready.toBoolean) {
          dut.clockDomain.waitSampling(1)
        }
      }

      // Overwrite the pmpcfg# registers.
      for (idx <- Range(0, count, 4)) {
        dut.io.write.valid #= true
        dut.io.config #= true
        dut.io.index #= idx
        dut.io.write.payload #= BigInt("03030303", 16)
        dut.clockDomain.waitSampling(1)
        while (!dut.io.write.ready.toBoolean) {
          dut.clockDomain.waitSampling(1)
        }
      }
      
      // Check all but the last pmpcfg# registers.
      if (count > 4) {
        for (idx <- Range(0, count - 4, 4)) {
          dut.io.write.valid #= false
          dut.io.config #= true
          dut.io.index #= idx
          dut.clockDomain.waitSampling(1)
          assert(dut.io.read.toBigInt == BigInt("87888789", 16), 
            "dut.io.readData missmatch")
        }
      }

      // Check the last pmpcfg#. Only the last pmp#cfg should be unlocked.
      dut.io.write.valid #= false
      dut.io.config #= true
      dut.io.index #= count - 1
      dut.clockDomain.waitSampling(1)
      assert(dut.io.read.toBigInt == BigInt("03888789", 16), 
        "dut.io.readData missmatch")
      
      // Write the pmpaddr# registers.
      for (idx <- 0 until count) {
        dut.io.write.valid #= true
        dut.io.config #= false
        dut.io.index #= idx
        dut.io.write.payload #= vec1(idx)
        dut.clockDomain.waitSampling(1)
        while (!dut.io.write.ready.toBoolean) {
          dut.clockDomain.waitSampling(1)
        }
      }

      // Check all but the last pmpaddr# registers.
      for (idx <- 0 until count - 1) {
        dut.io.write.valid #= false
        dut.io.config #= false
        dut.io.index #= idx
        dut.clockDomain.waitSampling(1)
        assert(dut.io.read.toBigInt == BigInt(vec0(idx)),
          "dut.io.readData missmatch")
      }

      // Check the last pmpaddr#. Only this one should be overwritten.
      dut.io.write.valid #= false
      dut.io.config #= false
      dut.io.index #= count - 1
      dut.clockDomain.waitSampling(1)
      assert(dut.io.read.toBigInt == BigInt(vec1(count - 1)),
        "dut.io.readData missmatch")

    }
  }

  test("tor") {
    compiled.doSim(seed = 2) { dut =>
      dut.clockDomain.forkStimulus(10)

      // Write the pmpcfg# registers.
      for (idx <- Range(0, count, 4)) {
        dut.io.write.valid #= true
        dut.io.config #= true
        dut.io.index #= idx
        dut.io.write.payload #= BigInt("08080808", 16)
        dut.clockDomain.waitSampling(1)
        while (!dut.io.write.ready.toBoolean) {
          dut.clockDomain.waitSampling(1)
        }
      }

      // Write the pmpaddr# registers.
      for (idx <- 0 until count) {
        dut.io.write.valid #= true
        dut.io.config #= false
        dut.io.index #= idx
        dut.io.write.payload #= 4 << idx
        dut.clockDomain.waitSampling(1)
        while (!dut.io.write.ready.toBoolean) {
          dut.clockDomain.waitSampling(1)
        }
      }

      // Check all but the first pmpaddr# registers.
      for (idx <- 1 until count) {
        assert(dut.boundLo(idx).toBigInt == BigInt(32 << (idx - 3)), 
          "dut.boundLo(idx) missmatch")
        // assert(dut.boundHi(idx).toBigInt == BigInt(32 << (idx - 2)), 
        //   "dut.boundHi(idx) missmatch")
      }

    }
  }

  test("na4") {
    compiled.doSim(seed = 2) { dut =>
      dut.clockDomain.forkStimulus(10)

    }
  }

  test("napot") {
    compiled.doSim(seed = 2) { dut =>
      dut.clockDomain.forkStimulus(10)

    }
  }
}