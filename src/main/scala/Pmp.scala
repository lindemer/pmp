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

case class PmpCell(preset : Bits = B"8'0") extends Bundle {
  val r = preset(0)
  val w = preset(1)
  val x = preset(2)
  val a = preset(4 downto 3)
  val l = preset(7)

  override def asBits : Bits = {
    B(8 bits, 7 -> l, (4 downto 3) -> a, 2 -> x, 1 -> w, 0 -> r)
  }
}

class PmpConf(preset : Bits = B"32'0") extends Bundle {
  val cells = preset.subdivideIn(8 bits)
      .reverse.map(x => new PmpCell(x))

  def toMask : Bits = {
    val locks = cells.map(info => info.l)
    locks.map(l => ~(l ## l ## l ## l ## l ## l ## l ## l))
      .reverse.foldLeft(Bits(0 bits))(_ ## _)
  }

  override def asBits : Bits = {
    cells.map(info => info.asBits)
      .reverse.foldLeft(Bits(0 bits))(_ ## _)
  }
}

class Pmp(configs : Int) extends Component {
  assert(configs % 4 == 0)

  val io = new Bundle {
    val write, select = in Bool
    val index = in UInt(4 bits)
    val readData = out Bits(32 bits)
    val writeData = in Bits(32 bits)
  }

  val pmpConfs = Mem(new PmpConf(), configs / 4)
  val pmpAddrs = Mem(Bits(32 bits), configs)

  val conf = pmpConfs.readAsync(io.index(3 downto 2), readFirst)

  when(io.select) {

    pmpConfs.write(
      io.index(3 downto 2),
      new PmpConf(io.writeData),
      io.write,
      conf.toMask
    )
    io.readData := conf.asBits

  } otherwise {

    val locked = conf.cells(io.index(1 downto 0)).l
    pmpAddrs.write(
      io.index,
      io.writeData,
      io.write && ~locked
    )
    io.readData := pmpAddrs.readAsync(io.index, readFirst)

  }
}