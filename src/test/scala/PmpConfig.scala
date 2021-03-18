package config

import spinal.core._
import spinal.core.SpinalConfig
import spinal.core.sim.SimConfig

object PmpConfig {
  val spinalConfig = SpinalConfig(
    defaultClockDomainFrequency = FixedFrequency(10 MHz),
    targetDirectory = "rtl"
  )

  def apply() = SimConfig.withWave
                    .withConfig(spinalConfig)
                    .workspacePath("waves")
}