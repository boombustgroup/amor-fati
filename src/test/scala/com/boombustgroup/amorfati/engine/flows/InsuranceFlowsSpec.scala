package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.agents.Insurance
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InsuranceFlowsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams          = SimParams.defaults
  private given RuntimeLedgerTopology = RuntimeLedgerTopology.zeroPopulation

  private val baseInput = InsuranceFlows.Input(
    employed = 80000,
    wage = PLN(7000.0),
    unempRate = Share(0.05),
    currentLifeReserves = PLN(90000000.0),
    currentNonLifeReserves = PLN(10000000.0),
    prevGovBondHoldings = PLN(50000000.0),
    prevCorpBondHoldings = PLN(20000000.0),
    prevEquityHoldings = PLN(10000000.0),
    govBondYield = Rate(0.06),
    corpBondYield = Rate(0.08),
    equityReturn = Rate(0.01),
  )

  "InsuranceFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = InsuranceFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have insurance net = premiums - claims + investment income" in {
    val flows    = InsuranceFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val premiums  =
      flows.filter(f => f.mechanism == FlowMechanism.InsLifePremium.toInt || f.mechanism == FlowMechanism.InsNonLifePremium.toInt).map(_.amount).sum
    val claims    = flows.filter(f => f.mechanism == FlowMechanism.InsLifeClaim.toInt || f.mechanism == FlowMechanism.InsNonLifeClaim.toInt).map(_.amount).sum
    val invIncome = flows.filter(_.mechanism == FlowMechanism.InsInvestmentIncome.toInt).map(_.amount).sum

    balances(InsuranceFlows.INS_ACCOUNT) shouldBe (premiums - claims + invIncome)
  }

  it should "match old Insurance.step premium and claim amounts" in {
    val prev   = Insurance.initial
    val oldIns = Insurance.step(prev, 80000, PLN(7000.0), Share(0.05), Rate(0.06), Rate(0.08), Rate(0.01), prev.corpBondHoldings)
    val flows  = InsuranceFlows.emit(baseInput)

    val newLifePrem    = flows.filter(_.mechanism == FlowMechanism.InsLifePremium.toInt).map(_.amount).sum
    val newNonLifePrem = flows.filter(_.mechanism == FlowMechanism.InsNonLifePremium.toInt).map(_.amount).sum
    val newLifeCl      = flows.filter(_.mechanism == FlowMechanism.InsLifeClaim.toInt).map(_.amount).sum
    val newNonLifeCl   = flows.filter(_.mechanism == FlowMechanism.InsNonLifeClaim.toInt).map(_.amount).sum

    PLN.fromRaw(newLifePrem) shouldBe oldIns.lastLifePremium
    PLN.fromRaw(newNonLifePrem) shouldBe oldIns.lastNonLifePremium
    PLN.fromRaw(newLifeCl) shouldBe oldIns.lastLifeClaims
    PLN.fromRaw(newNonLifeCl) shouldBe oldIns.lastNonLifeClaims
  }

  it should "route investment income through reserve assets rather than cash" in {
    val expectedInvIncome  =
      baseInput.prevGovBondHoldings * baseInput.govBondYield.monthly +
        baseInput.prevCorpBondHoldings * baseInput.corpBondYield.monthly +
        baseInput.prevEquityHoldings * baseInput.equityReturn
    val totalReserves      = baseInput.currentLifeReserves + baseInput.currentNonLifeReserves
    val lifeShare          =
      if totalReserves > PLN.Zero then Share(baseInput.currentLifeReserves / totalReserves) else Share(0.5)
    val expectedLifeInv    = expectedInvIncome * lifeShare
    val expectedNonLifeInv = expectedInvIncome - expectedLifeInv
    val batches            = InsuranceFlows.emitBatches(baseInput)
    val invBatches         = batches.filter(_.mechanism == FlowMechanism.InsInvestmentIncome)

    invBatches should have size 2
    invBatches.map(_.asset).toSet shouldBe Set(AssetType.LifeReserve, AssetType.NonLifeReserve)
    invBatches.exists(batch => batch.asset == AssetType.Cash) shouldBe false
    invBatches.map(RuntimeLedgerTopology.totalTransferred).sum shouldBe expectedInvIncome.toLong
    RuntimeLedgerTopology.totalTransferred(invBatches.find(_.asset == AssetType.LifeReserve).get) shouldBe expectedLifeInv.toLong
    RuntimeLedgerTopology.totalTransferred(invBatches.find(_.asset == AssetType.NonLifeReserve).get) shouldBe expectedNonLifeInv.toLong
  }

  it should "preserve SFC across 120 months" in {
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, InsuranceFlows.emit(baseInput))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
