/*
 * Copyright (c) 2021 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package pmp

import spinal.core.sim._
import spinal.core._
import spinal.lib._

/* Each 32-bit pmpcfg# register contains four 8-bit configuration sections.
 * These section numbers contain flags which apply to regions defined by the
 * corresponding pmpaddr# register.
 *
 *    3                   2                   1
 *  1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |    pmp3cfg    |    pmp2cfg    |    pmp1cfg    |    pmp0cfg    | pmpcfg0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |    pmp7cfg    |    pmp6cfg    |    pmp5cfg    |    pmp4cfg    | pmpcfg2
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *     7       6       5       4       3       2       1       0
 * +-------+-------+-------+-------+-------+-------+-------+-------+
 * |   L   |       0       |       A       |   X   |   W   |   R   | pmp#cfg
 * +-------+-------+-------+-------+-------+-------+-------+-------+
 *
 *	  L: locks configuration until system reset (including M-mode)
 *	  0: hardwired to zero
 *	  A: 0 = OFF (null region / disabled)
 *	     1 = TOR (top of range)
 * 	     2 = NA4 (naturally aligned four-byte region)
 *	     3 = NAPOT (naturally aligned power-of-two region, > 7 bytes)
 *	  X: execute
 *	  W: write
 *	  R: read
 *
 * TOR: Each 32-bit pmpaddr# register defines the upper bound of the pmp region
 * right-shifted by two bits. The lower bound of the region is the previous
 * pmpaddr# register. In the case of pmpaddr0, the lower bound is address 0x0.
 *
 *    3                   2                   1
 *  1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |                        address[33:2]                          | pmpaddr#
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 * NAPOT: Each 32-bit pmpaddr# register defines the region address and the size
 * of the pmp region. The number of concurrent 1s begging at the LSB indicates
 * the size of the region as a power of two (e.g. 0x...0 = 8-byte, 0x...1 =
 * 16-byte, 0x...11 = 32-byte, etc.).
 *
 *    3                   2                   1
 *  1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |                        address[33:2]                |0|1|1|1|1| pmpaddr#
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 * NA4: This is essentially an edge case of NAPOT where the entire pmpaddr#
 * register defines a 4-byte wide region.
 */

trait Pmp {
  def OFF = 0
  def TOR = 1
  def NA4 = 2
  def NAPOT = 3

  def xlen = 32
  def rBit = 0
  def wBit = 1
  def xBit = 2
  def aBits = 4 downto 3
  def lBit = 7
}

class PmpSetter() extends Component with Pmp {
  val io = new Bundle {
    val a = in Bits(2 bits)
    val addr = in UInt(xlen bits)
    val prevHi = in UInt(30 bits)
    val boundLo, boundHi = out UInt(30 bits)
  }

  val shifted = io.addr(31 downto 2)
  io.boundLo := shifted
  io.boundHi := shifted

  switch (io.a) {
    is (TOR) {
      io.boundLo := io.prevHi
    }
    is (NA4) {
      io.boundHi := shifted + 1
    }
    is (NAPOT) {
      val mask = io.addr & ~(io.addr + 1)
      val boundLo = (io.addr ^ mask)(29 downto 0)
      io.boundLo := boundLo
      io.boundHi := boundLo + ((mask + 1) |<< 3)(29 downto 0)
    }
  }
}

class PmpController(count : Int) extends Component with Pmp {
  assert(count % 4 == 0)
  assert(count <= 16)

  val pmpaddr = Mem(UInt(xlen bits), count)
  val pmpcfg = Reg(Bits(8 * count bits)) init(0)
  val boundLo, boundHi = Mem(UInt(30 bits), count) simPublic()

  val io = new Bundle {
    val config = in Bool
    val index = in UInt(log2Up(count) bits)
    val read = out Bits(xlen bits)
    val write = slave Stream(Bits(xlen bits))
  }

  val cfgSel = io.index(log2Up(count) - 1 downto 2)
  val pmpcfgs = pmpcfg.subdivideIn(xlen bits)
  val pmpcfgN = pmpcfgs(cfgSel)
  val pmpNcfg = pmpcfgN.subdivideIn(8 bits)
  
  val csr = new Area {
    when (io.config) {
      when (io.write.valid) {
        switch(cfgSel) {
          for (i <- 0 until (count / 4)) {
            is(i) {
              for (j <- Range(0, xlen, 8)) {
                val bitRange = j + xlen * i + lBit downto j + xlen * i
                val overwrite = io.write.payload.subdivideIn(8 bits)(j / 8)
                val locked = pmpcfgs(i).subdivideIn(8 bits)(j / 8)(lBit)
                when (~locked) {
                  pmpcfg(bitRange).assignFromBits(overwrite)
                  if (j != 0 || i != 0) {
                    when (overwrite(lBit) & overwrite(aBits) === TOR) {
                      pmpcfg(j + xlen * i - 1) := True
                    }
                  }
                }
              }
            }
          }
        }
      }
      io.read.assignFromBits(pmpcfgN)
    } otherwise {
      when (io.write.valid) {
        val lock = pmpNcfg(io.index(1 downto 0))(lBit)
        pmpaddr.write(
          io.index,
          io.write.payload.asUInt,
          io.write.valid & ~io.config & ~lock
        )
      }
      io.read := pmpaddr.readAsync(io.index).asBits
    }
  }

  val pipeline = new Area {
    val setter = new PmpSetter()
    val enable = RegInit(False)
    val counter = Reg(UInt(log2Up(count) bits))
    val setNext = RegInit(False)
    
    when (io.config) {
      when (io.write.valid & ~enable) {
        enable := True
        io.write.ready := False
        counter := io.index(log2Up(count) - 1 downto 2) @@ U"2'00"
      }.elsewhen (enable) {
        counter := counter + 1
        when (counter(1 downto 0) === 3) {
          enable := False
          io.write.ready := True
        } otherwise {
          io.write.ready := False
        }
      } otherwise {
        io.write.ready := True
      }
    } otherwise {
      when (io.write.valid & ~enable) {
        enable := True
        counter := io.index
        io.write.ready := False
        when (io.index =/= (count - 1)) {
          setNext := True
        } otherwise {
          setNext := False
        }
      }.elsewhen (setNext) {
        io.write.ready := False
        counter := counter + 1
        setNext := False
      } otherwise {
        enable := False
        io.write.ready := True
      }
    }

    val sel = counter(log2Up(count) - 1 downto 2)
    setter.io.a := pmpcfgs(sel).subdivideIn(8 bits)(counter(1 downto 0))(aBits)
    when (counter === 0) {
      setter.io.prevHi := 0
    } otherwise {
      setter.io.prevHi := boundHi(counter - 1)
    }
    setter.io.addr := pmpaddr(counter)
    when (enable) {
      boundLo(counter) := setter.io.boundLo
      boundHi(counter) := setter.io.boundHi
    }
  }
}