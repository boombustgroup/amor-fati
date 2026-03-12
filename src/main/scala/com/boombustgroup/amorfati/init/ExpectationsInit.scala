package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.Expectations

/** Factory for expectations state initialization. */
object ExpectationsInit:

  def create()(using SimParams): Expectations.State = Expectations.initial
