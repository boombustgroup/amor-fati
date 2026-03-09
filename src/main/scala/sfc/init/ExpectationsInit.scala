package sfc.init

import sfc.config.SimParams
import sfc.engine.mechanisms.Expectations

/** Factory for expectations state initialization. */
object ExpectationsInit:

  def create()(using p: SimParams): Expectations.State =
    if p.flags.expectations then Expectations.initial
    else Expectations.zero
