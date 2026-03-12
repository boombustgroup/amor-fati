package com.boombustgroup.amorfati.config

/** Simulation time horizon.
  *
  * @param duration
  *   number of monthly time-steps to simulate (default 120 = 10 years)
  */
case class TimelineConfig(
    duration: Int = 120,
)
