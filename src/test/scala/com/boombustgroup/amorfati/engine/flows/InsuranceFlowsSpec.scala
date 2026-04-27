package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
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
    wage = PLN(7000),
    unempRate = Share.decimal(5, 2),
    currentLifeReserves = PLN(90000000),
    currentNonLifeReserves = PLN(10000000),
    prevGovBondHoldings = PLN(50000000),
    prevCorpBondHoldings = PLN(20000000),
    corpBondDefaultLoss = PLN.Zero,
    prevEquityHoldings = PLN(10000000),
    govBondYield = Rate.decimal(6, 2),
    corpBondYield = Rate.decimal(8, 2),
    equityReturn = Rate.decimal(1, 2),
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

  it should "match Insurance.step premium and claim amounts" in {
    val opening = Insurance.OpeningBalances(
      lifeReserves = baseInput.currentLifeReserves,
      nonLifeReserves = baseInput.currentNonLifeReserves,
      govBondHoldings = baseInput.prevGovBondHoldings,
      corpBondHoldings = baseInput.prevCorpBondHoldings,
      equityHoldings = baseInput.prevEquityHoldings,
    )
    val oldIns  =
      Insurance.step(
        Insurance.StepInput(
          opening = opening,
          employed = 80000,
          wage = PLN(7000),
          unempRate = Share.decimal(5, 2),
          govBondYield = Rate.decimal(6, 2),
          corpBondYield = Rate.decimal(8, 2),
          equityReturn = Rate.decimal(1, 2),
          corpBondDefaultLoss = PLN.Zero,
        ),
      )
    val flows   = InsuranceFlows.emit(baseInput)

    val newLifePrem    = flows.filter(_.mechanism == FlowMechanism.InsLifePremium.toInt).map(_.amount).sum
    val newNonLifePrem = flows.filter(_.mechanism == FlowMechanism.InsNonLifePremium.toInt).map(_.amount).sum
    val newLifeCl      = flows.filter(_.mechanism == FlowMechanism.InsLifeClaim.toInt).map(_.amount).sum
    val newNonLifeCl   = flows.filter(_.mechanism == FlowMechanism.InsNonLifeClaim.toInt).map(_.amount).sum

    PLN.fromRaw(newLifePrem) shouldBe oldIns.state.lastLifePremium
    PLN.fromRaw(newNonLifePrem) shouldBe oldIns.state.lastNonLifePremium
    PLN.fromRaw(newLifeCl) shouldBe oldIns.state.lastLifeClaims
    PLN.fromRaw(newNonLifeCl) shouldBe oldIns.state.lastNonLifeClaims
  }

  it should "route investment income through reserve assets rather than cash" in {
    val expectedInvIncome  =
      baseInput.prevGovBondHoldings * baseInput.govBondYield.monthly +
        baseInput.prevCorpBondHoldings * baseInput.corpBondYield.monthly +
        baseInput.prevEquityHoldings * baseInput.equityReturn -
        baseInput.corpBondDefaultLoss
    val totalReserves      = baseInput.currentLifeReserves + baseInput.currentNonLifeReserves
    val lifeShare          =
      if totalReserves > PLN.Zero then shareBD(baseInput.currentLifeReserves / totalReserves) else Share.decimal(5, 1)
    val expectedLifeInv    = expectedInvIncome * lifeShare
    val expectedNonLifeInv = expectedInvIncome - expectedLifeInv
    val batches            = InsuranceFlows.emitBatches(baseInput)
    val invBatches         = batches.filter(_.mechanism == FlowMechanism.InsInvestmentIncome)

    invBatches should have size 2
    invBatches.map(_.asset).toSet shouldBe Set(AssetType.LifeReserve, AssetType.NonLifeReserve)
    invBatches.exists(batch => batch.asset == AssetType.Cash) shouldBe false
    all(invBatches.map(_.from)) shouldBe EntitySector.Insurance
    all(invBatches.map(_.to)) shouldBe EntitySector.Insurance
    invBatches.map(RuntimeLedgerTopology.totalTransferred).sum shouldBe expectedInvIncome.toLong
    RuntimeLedgerTopology.totalTransferred(invBatches.find(_.asset == AssetType.LifeReserve).get) shouldBe expectedLifeInv.toLong
    RuntimeLedgerTopology.totalTransferred(invBatches.find(_.asset == AssetType.NonLifeReserve).get) shouldBe expectedNonLifeInv.toLong
  }

  it should "keep reserve assets inside the insurance runtime sector" in {
    val batches        = InsuranceFlows.emitBatches(baseInput)
    val reserveBatches = batches.filter(batch => batch.asset == AssetType.LifeReserve || batch.asset == AssetType.NonLifeReserve)

    reserveBatches should not be empty
    all(reserveBatches.map(_.from)) shouldBe EntitySector.Insurance
    all(reserveBatches.map(_.to)) shouldBe EntitySector.Insurance
    val aggregate      = summon[RuntimeLedgerTopology].insurance.aggregate
    val persistedOwner = summon[RuntimeLedgerTopology].insurance.persistedOwner
    reserveBatches.foreach:
      case broadcast: BatchedFlow.Broadcast =>
        if broadcast.mechanism == FlowMechanism.InsLifeClaim || broadcast.mechanism == FlowMechanism.InsNonLifeClaim then
          broadcast.fromIndex shouldBe persistedOwner
          broadcast.targetIndices.head shouldBe aggregate
        else
          broadcast.fromIndex shouldBe aggregate
          broadcast.targetIndices.head shouldBe persistedOwner
      case other                            => fail(s"Expected reserve broadcast, got $other")
  }

  it should "route corporate bond default losses as negative reserve investment income" in {
    val lossInput       = baseInput.copy(corpBondDefaultLoss = PLN(2000000))
    val grossInvestment =
      lossInput.prevGovBondHoldings * lossInput.govBondYield.monthly +
        lossInput.prevCorpBondHoldings * lossInput.corpBondYield.monthly +
        lossInput.prevEquityHoldings * lossInput.equityReturn
    val expectedLoss    = lossInput.corpBondDefaultLoss - grossInvestment
    val batches         = InsuranceFlows.emitBatches(lossInput)
    val lossLegs        = batches.filter(_.mechanism == FlowMechanism.InsInvestmentIncome)

    lossLegs should have size 2
    all(lossLegs.map(_.from)) shouldBe EntitySector.Insurance
    all(lossLegs.map(_.to)) shouldBe EntitySector.Insurance
    lossLegs.foreach:
      case broadcast: BatchedFlow.Broadcast =>
        broadcast.fromIndex shouldBe summon[RuntimeLedgerTopology].insurance.persistedOwner
        broadcast.targetIndices.head shouldBe summon[RuntimeLedgerTopology].insurance.aggregate
      case other                            => fail(s"Expected reserve loss broadcast, got $other")
    lossLegs.map(RuntimeLedgerTopology.totalTransferred).sum shouldBe expectedLoss.toLong
  }
