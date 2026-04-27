package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixEvidence.MatrixEvidenceBundle
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}

private[matrix] object SfcMatrixEvidenceTestSupport:

  private val EvidenceSeed      = 1L
  private val EvidenceMonthSeed = 1001L

  def deterministicStep()(using SimParams): FlowSimulation.StepOutput =
    val init = WorldInit.initialize(InitRandomness.Contract.fromSeed(EvidenceSeed))
    FlowSimulation.step(FlowSimulation.SimState.fromInit(init), MonthRandomness.Contract.fromSeed(EvidenceMonthSeed))

  def deterministicBundle(commit: String)(using SimParams): MatrixEvidenceBundle =
    bundleFrom(deterministicStep(), commit)

  def bundleFrom(step: FlowSimulation.StepOutput, commit: String)(using SimParams): MatrixEvidenceBundle =
    MatrixEvidenceBundle.fromStep(seed = EvidenceSeed, step = step, commit = commit)

end SfcMatrixEvidenceTestSupport
