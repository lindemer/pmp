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
  def rbit = 0
  def wbit = 1
  def xbit = 2
  def abits = 4 downto 3
  def lbit = 7
}

class PmpRegion() extends Bundle {
  val lbound, rbound = UInt(30 bits)
  val valid = False
}

class PmpSetter() extends Component with Pmp {
  val io = new Bundle {
    val a = in UInt(2 bits)
    val addr = in UInt(word bits)
    val prev = in UInt(30 bits)
    val region = out(new PmpRegion())
  }

  io.region.lbound := io.addr(31 downto 2)
  io.region.rbound := io.addr(31 downto 2)

  switch (io.a) {
    is (TOR) {
      io.region.lbound := io.prev
    }
    is (NA4) {
      io.region.rbound := (io.addr + 4)(31 downto 2)
    }
    is (NAPOT) {
      val mask = io.addr & ~(io.addr + 1)
      val lbound = (io.addr ^ mask)(31 downto 2)
      io.region.lbound := lbound
      io.region.rbound := lbound + ((mask + 1)(31 downto 2) |<< 1)
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

  val addrs = Mem(UInt(word bits), count)
  val cfgs = Reg(Bits(8 * count bits)) init(0)
  val sel = io.index(log2Up(count) - 1 downto 2)
  val cfg = cfgs.subdivideIn(word bits)(sel)
  
  val csr = new Area {
    when (io.select) {
      when (io.write.valid) {
        switch(sel) {
          for (i <- 0 until (count / 4)) {
            is(i) {
              for (j <- Range(0, word, 8)) {
                val readRange = j + lbit downto j
                val writeRange = j + word * i + lbit downto j + word * i
                val locked = cfgs(j + word * i + lbit)
                when (~locked) {
                  val newCfg = io.write.payload.asBits(readRange)
                  cfgs(writeRange).assignFromBits(newCfg)
                  if (j != 0 || i != 0) {
                    when (newCfg(lbit) && newCfg(abits) === TOR) {
                      cfgs(j + word * i - 1) := True
                    }
                  }
                }
              }
            }
          }
        }
      }
      io.read.assignFromBits(cfg)
    } otherwise {
      when (io.write.valid) {
        val lock = cfg.subdivideIn(8 bits)(io.index(1 downto 0))(lbit)
        addrs.write(
          io.index,
          io.write.payload,
          io.write.valid && ~io.select && ~lock
        )
      }
      io.read := addrs.readAsync(io.index)
    }
  }

  val counter = UInt(3 bits)
  io.write.ready := True

  val regions = Mem(new PmpRegion(), count)

  val pipeline = new Area {

    val index = Reg(UInt(log2Up(count) bits))
    val setter = new PmpSetter()

    index := io.index

  }

  // stall the region computation 1 cycle to wait for writes
  //val regionSelect = Reg(UInt(2 bits))
  //regionSelect := cfgSelect


  /*
   * Pipeline:
   *  (0) write to pmpcfg#, move setter, not ready
   *  (1) fix region # + 0, move setter, not ready
   *  (2) fix region # + 1, move setter, not ready
   *  (3) fix region # + 2, move setter, not ready
   *  (4) fix region # + 3, ready
   * OR
   *  (0) write to pmpaddr#, move setter, not ready
   *  (1) fix region #, ready
   */
}