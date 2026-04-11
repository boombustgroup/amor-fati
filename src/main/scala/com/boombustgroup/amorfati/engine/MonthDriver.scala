package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.FlowSimulation

/** Shared unfold-style driver over the explicit monthly step boundary.
  *
  * This is orchestration glue at the engine level: callers own the schedule of
  * month-level randomness contracts, while [[FlowSimulation.step]] remains the
  * narrow one-month transition.
  */
object MonthDriver:

  type SimState   = FlowSimulation.SimState
  type StepOutput = FlowSimulation.StepOutput

  /** Caller-owned month schedule for the engine unfold.
    *
    * Returning `Some(contract)` executes one monthly step from the provided
    * state boundary. Returning `None` closes the unfold.
    */
  type RandomnessSchedule = SimState => Option[MonthRandomness.Contract]

  def unfoldSteps(initialState: SimState)(schedule: RandomnessSchedule)(using p: SimParams): Iterator[StepOutput] =
    Iterator.unfold(initialState): state =>
      schedule(state).map: randomness =>
        val output = FlowSimulation.step(state, randomness)
        (output, output.nextState)
