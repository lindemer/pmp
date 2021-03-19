package pmp

import spinal.core._

object PmpMain {
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVhdl(new Pmp(count = 16))
  }
}