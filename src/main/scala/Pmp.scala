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

class PmpCfg(preset : UInt = U"32'0") extends Bundle {
  val encoded = preset

  def cfg = encoded.subdivideIn(8 bits)
  def r = cfg.map(c => c(0))
  def w = cfg.map(c => c(1))
  def x = cfg.map(c => c(2))
  def a = cfg.map(c => c(4 downto 3))
  def l = cfg.map(c => c(7))
  
  def asMask : Bits = { 
    val mask = l.map(l => l ? B"8'x00" | B"8'xff")
    mask(0) ## mask(1) ## mask(2) ## mask(3)
  }
}

class Pmp(count : Int) extends Component {
  assert(count % 4 == 0)
  assert(count <= 16)

  val io = new Bundle {
    val write, select = in Bool
    val index = in UInt(log2Up(count) bits)
    val writeData = in UInt(32 bits)
    val readData = out UInt(32 bits)
  }
  val cfgs = Mem(UInt(32 bits), Array.fill(count / 4)(U"32'0"))
  val addrs = Mem(UInt(30 bits), count)

  val cfg = cfgs.readAsync(count match {
    case 4 => U"0"
    case _ => io.index((log2Up(count) - 1) downto 2)
  })

  cfgs.write(
    count match {
      case 4 => U"0"
      case _ => io.index((log2Up(count) - 3) downto 0)
    },
    io.writeData,
    io.write && io.select,
    ~(cfg(31) ## B"b11" ## cfg(31) ## cfg(31) ## cfg(31) ## cfg(31) ## cfg(31) ## 
      cfg(23) ## B"b11" ## cfg(23) ## cfg(23) ## cfg(23) ## cfg(23) ## cfg(23) ## 
      cfg(15) ## B"b11" ## cfg(15) ## cfg(15) ## cfg(15) ## cfg(15) ## cfg(15) ## 
      cfg( 7) ## B"b11" ## cfg( 7) ## cfg( 7) ## cfg( 7) ## cfg( 7) ## cfg( 7))
  ) 
  
  val locked = cfg.subdivideIn(8 bits)(io.index(1 downto 0))(7)
  addrs.write(
    io.index,
    io.writeData(29 downto 0),
    io.write && ~io.select && ~locked 
  )

  when(io.select) {
    io.readData := cfg
  } otherwise {
    io.readData := U"2'00" @@ addrs.readSync(io.index)
  }
}