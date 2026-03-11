package sfc.init

import sfc.config.SimParams
import sfc.engine.mechanisms.Expectations

/** Factory for expectations state initialization. */
object ExpectationsInit:

  def create()(using SimParams): Expectations.State = Expectations.initial
