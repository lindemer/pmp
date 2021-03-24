package pmp

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

  def word = 32
  def rBit = 0
  def wBit = 1
  def xBit = 2
  def aBits = 4 downto 3
  def lBit = 7
}

class PmpRegion() extends Bundle {
  val lBound, rBound = UInt(30 bits)
  val valid = Bool
}

class PmpSetter() extends Component with Pmp {
  val io = new Bundle {
    val a = in Bits(2 bits)
    val addr = in UInt(word bits)
    val prev = in UInt(30 bits)
    val region = out(new PmpRegion())
  }

  val shifted = io.addr(31 downto 2)
  io.region.lBound := shifted
  io.region.rBound := shifted
  io.region.valid := True

  switch (io.a) {
    is (TOR) {
      io.region.lBound := io.prev
    }
    is (NA4) {
      io.region.rBound := shifted + 1
    }
    is (NAPOT) {
      val mask = io.addr & ~(io.addr + 1)
      val lBound = (io.addr ^ mask)(31 downto 2)
      io.region.lBound := lBound
      io.region.rBound := lBound + ((mask + 1)(31 downto 2) |<< 1)
    }
    default {
      io.region.valid := False
    }
  }
}

class PmpController(count : Int) extends Component with Pmp {
  assert(count % 4 == 0)
  assert(count <= 16)

  val io = new Bundle {
    val select = in Bool
    val index = in UInt(log2Up(count) bits)
    val read = out UInt(word bits)
    val write = slave Stream(UInt(word bits))
  }

  val pmpaddr = Mem(UInt(word bits), count)
  val pmpcfg = Reg(Bits(8 * count bits)) init(0)
  val cfgSelect = io.index(log2Up(count) - 1 downto 2)
  val pmpcfgN = pmpcfg.subdivideIn(word bits)(cfgSelect)
  val pmpNcfg = pmpcfgN.subdivideIn(8 bits)
  
  val csr = new Area {
    when (io.select) {
      when (io.write.valid) {
        switch(cfgSelect) {
          for (i <- 0 until (count / 4)) {
            is(i) {
              for (j <- Range(0, word, 8)) {
                val readRange = j + lBit downto j
                val writeRange = j + word * i + lBit downto j + word * i
                val locked = pmpcfg(j + word * i + lBit)
                when (~locked) {
                  val newCfg = io.write.payload.asBits(readRange)
                  pmpcfg(writeRange).assignFromBits(newCfg)
                  if (j != 0 || i != 0) {
                    when (newCfg(lBit) & newCfg(aBits) === TOR) {
                      pmpcfg(j + word * i - 1) := True
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
          io.write.payload,
          io.write.valid & ~io.select & ~lock
        )
      }
      io.read := pmpaddr.readAsync(io.index)
    }
  }

  val regions = Mem(new PmpRegion(), count)

  val pipeline = new Area {
    val counter = Reg(UInt(2 bits)) init(0)
    val enable = RegInit(False)
    val setter = new PmpSetter()
    val index = cfgSelect @@ counter
    
    when (io.select) {
      when (io.write.valid & ~enable) {
        enable := True
        io.write.ready := False
      }.elsewhen (enable) {
        counter := counter + 1
        when (counter === 3) {
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
        counter := io.index(1 downto 0)
        io.write.ready := False
      } otherwise {
        enable := False
        counter := 0
        io.write.ready := True
      }
    }

    setter.io.a := pmpNcfg(counter)(aBits)
    when (index === 0) {
      setter.io.prev := 0
    } otherwise {
      setter.io.prev := regions(index - 1).rBound 
    }
    setter.io.addr := pmpaddr(index)
    when (enable) {
      regions(index) := setter.io.region
    }
  }
}