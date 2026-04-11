package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.engine.economics.WorldAssemblyEconomics
import com.boombustgroup.amorfati.engine.flows.FlowSimulation.{MonthlyCalculus, PostMonth, SemanticFlowInputs, SignalBoundaryInputs}

/** Tiny type-level timeline for one monthly engine step.
  *
  * `At[A, P]` is intentionally zero-cost at runtime: it only tags values with a
  * phase so internal boundaries in [[FlowSimulation]] stay explicit.
  */
object MonthSemantics:

  /** Marker hierarchy for the coarse month timeline. */
  sealed trait Phase

  /** Persisted decision surface visible at the start of month `t`. */
  sealed trait Pre extends Phase

  /** Same-month artifacts used during the execution of month `t`. */
  sealed trait SameMonth extends Phase

  /** Post-assembly state after month `t` has been realized, before reuse as
    * input.
    */
  sealed trait Post extends Phase

  /** Extracted seed that becomes the `pre` surface for month `t+1`. */
  sealed trait NextPre extends Phase

  /** Zero-cost phase tag. The underlying runtime representation stays `A`. */
  opaque type At[+A, P <: Phase] = A

  object At:
    /** Constructor is kept engine-internal so phase tags are introduced
      * deliberately.
      */
    private[engine] inline def apply[A, P <: Phase](value: A): At[A, P] = value

    /** Explicit unwrap used at phase boundaries. */
    extension [A, P <: Phase](staged: At[A, P]) private[engine] inline def value: A = staged

  /** Start-of-month seed consumed by timing-sensitive blocks. */
  type SeedIn = At[DecisionSignals, Pre]

  /** Same-month operational signal surface derived inside the step. */
  type Operational = At[OperationalSignals, SameMonth]

  /** Same-month signal boundary reused by operational, timing, and seed
    * extraction.
    */
  type SignalView = At[SignalBoundaryInputs, SameMonth]

  /** Same-month flow translation plan consumed by batch emission. */
  type FlowPlan = At[MonthlyCalculus, SameMonth]

  /** Same-month payload narrowed for post-month world assembly. */
  type PostInputs = At[WorldAssemblyEconomics.StepInput, SameMonth]

  /** Same-month payload narrowed for executed-flow semantic projection. */
  type SemanticProjection = At[SemanticFlowInputs, SameMonth]

  /** Realized post-month assembly before extracting the next seed. */
  type PostAssembly = At[PostMonth, Post]

  /** Extracted next-month seed plus provenance. */
  type SeedOut = At[SignalExtraction.Output, NextPre]
