package config

import spinal.core.SpinalConfig
import spinal.core.sim.SimConfig

object PmpConfig {
  def apply() = SimConfig.withWave
                    .withConfig(SpinalConfig(targetDirectory = "rtl"))
                    .workspacePath("waves")
}