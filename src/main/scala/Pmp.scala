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

class Pmp(count : Int) extends Component {
  assert(count % 4 == 0)
  assert(count <= 16)

  val io = new Bundle {
    val write, select = in Bool
    val index = in UInt(log2Up(count) bits)
    val writeData = in UInt(32 bits)
    val readData = out UInt(32 bits)
  }

  val pmpAddr = Mem(UInt(32 bits), count)
  val pmpCfg = Reg(Bits(8 * count bits)) init(0)

  // io.index is a 2-bit CSR selector when accessing pmpCfg
  val csrNum = count match {
    case 4 => U"0"
    case _ => io.index(log2Up(count) - 3 downto 0)
  }
  val csr = pmpCfg.subdivideIn(32 bits)(csrNum)

  // io.select === 1 for access to pmpCfg
  when (io.select) {
    when (io.write) {

      // get the requested configuration CSR from the bit vector
      switch(csrNum) {
        for (i <- 0 until (count / 4)) {
          is(i) {
            
            // for each region in the configuration CSR
            for (j <- Range(0, 32, 8)) {

              // if the region isn't locked, overwrite it with io.writeData
              when (~pmpCfg(j + 7 + 32 * i)) {
                val readSet = j + 7 downto j
                val writeSet = j + 7 + 32 * i downto j + 32 * i
                pmpCfg(writeSet).assignFromBits(io.writeData.asBits(readSet))
              }
            }
          }
        }
      }
    }
    io.readData.assignFromBits(csr)

  // io.select === 0 for access to pmpAddr
  } otherwise {
    io.readData := pmpAddr.readAsync(io.index)
    when (io.write) {

      // check the lock bit on the corresponding configuration register
      val lock = csr.subdivideIn(8 bits)(io.index(1 downto 0))(7)
      pmpAddr.write(
        io.index,
        io.writeData,
        io.write && ~io.select && ~lock
      )
    }
  }


}