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

  /** Zero-cost phase tag. The underlying runtime representation stays `A`. Raw
    * tagging stays private to this object; callers use named boundary helpers
    * instead of arbitrary `At` construction or unwrapping.
    */
  opaque type At[+A, P <: Phase] = A

  private inline def wrap[A, P <: Phase](value: A): At[A, P] = value

  private inline def unwrap[A, P <: Phase](staged: At[A, P]): A = staged

  /** Start-of-month seed consumed by timing-sensitive blocks. */
  type SeedIn = At[DecisionSignals, Pre]

  inline def seedIn(signals: DecisionSignals): SeedIn =
    wrap[DecisionSignals, Pre](signals)

  extension (seedIn: SeedIn)
    inline def decisionSignals: DecisionSignals =
      unwrap(seedIn)

  /** Same-month operational signal surface derived inside the step. */
  type Operational = At[OperationalSignals, SameMonth]

  inline def operational(signals: OperationalSignals): Operational =
    wrap[OperationalSignals, SameMonth](signals)

  extension (operational: Operational)
    inline def operationalSignals: OperationalSignals =
      unwrap(operational)

  /** Same-month signal boundary reused by operational, timing, and seed
    * extraction.
    */
  type SignalView = At[SignalBoundaryInputs, SameMonth]

  private[engine] inline def signalView(signals: SignalBoundaryInputs): SignalView =
    wrap[SignalBoundaryInputs, SameMonth](signals)

  extension (signalView: SignalView)
    private[engine] inline def labor =
      unwrap(signalView).labor

    private[engine] inline def demand =
      unwrap(signalView).demand

  /** Same-month flow translation plan consumed by batch emission. */
  type FlowPlan = At[MonthlyCalculus, SameMonth]

  private[engine] inline def flowPlan(calculus: MonthlyCalculus): FlowPlan =
    wrap[MonthlyCalculus, SameMonth](calculus)

  extension (flowPlan: FlowPlan)
    inline def calculus: MonthlyCalculus =
      unwrap(flowPlan)

  /** Same-month payload narrowed for post-month world assembly. */
  type PostInputs = At[WorldAssemblyEconomics.StepInput, SameMonth]

  private[engine] inline def postInputs(input: WorldAssemblyEconomics.StepInput): PostInputs =
    wrap[WorldAssemblyEconomics.StepInput, SameMonth](input)

  extension (postInputs: PostInputs)
    private[engine] inline def assemblyInput: WorldAssemblyEconomics.StepInput =
      unwrap(postInputs)

  /** Same-month payload narrowed for executed-flow semantic projection. */
  type SemanticProjection = At[SemanticFlowInputs, SameMonth]

  private[engine] inline def semanticProjection(inputs: SemanticFlowInputs): SemanticProjection =
    wrap[SemanticFlowInputs, SameMonth](inputs)

  extension (semanticProjection: SemanticProjection)
    private[engine] inline def labor =
      unwrap(semanticProjection).labor

    private[engine] inline def hhIncome =
      unwrap(semanticProjection).hhIncome

    private[engine] inline def firms =
      unwrap(semanticProjection).firms

    private[engine] inline def hhFinancial =
      unwrap(semanticProjection).hhFinancial

    private[engine] inline def prices =
      unwrap(semanticProjection).prices

    private[engine] inline def openEcon =
      unwrap(semanticProjection).openEcon

    private[engine] inline def banking =
      unwrap(semanticProjection).banking

  /** Realized post-month assembly before extracting the next seed. */
  type PostAssembly = At[PostMonth, Post]

  inline def postAssembly(postMonth: PostMonth): PostAssembly =
    wrap[PostMonth, Post](postMonth)

  extension (post: PostAssembly)
    inline def assembled =
      unwrap(post).assembled

    inline def boundaryOut =
      unwrap(post).boundaryOut

    inline def timing =
      unwrap(post).timing

  /** Extracted next-month seed plus provenance. */
  type SeedOut = At[SignalExtraction.Output, NextPre]

  inline def seedOut(output: SignalExtraction.Output): SeedOut =
    wrap[SignalExtraction.Output, NextPre](output)

  extension (seedOut: SeedOut)
    inline def signalExtraction: SignalExtraction.Output =
      unwrap(seedOut)

    inline def nextSeed: DecisionSignals =
      unwrap(seedOut).seedOut

    inline def provenance: SeedOutProvenance =
      unwrap(seedOut).provenance
