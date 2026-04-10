package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BatchedEmissionContractSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  private def legacyMechanismTotals(flows: Vector[Flow]): Map[Int, Long] =
    flows.groupMapReduce(_.mechanism)(_.amount)(_ + _)

  private def batchedMechanismTotals(batches: Vector[BatchedFlow]): Map[Int, Long] =
    batches.groupMapReduce(_.mechanism.toInt)(AggregateBatchContract.totalTransferred)(_ + _)

  "emitBatches" should "preserve mechanism totals across migrated emitters" in {
    val legacyFlows = Vector.concat(
      ZusFlows.emit(ZusFlows.ZusInput(80000, PLN(7000.0), 1000)),
      NfzFlows.emit(NfzFlows.NfzInput(80000, PLN(7000.0), 90000, 1000)),
      PpkFlows.emit(PpkFlows.PpkInput(80000, PLN(7000.0))),
      EarmarkedFlows.emit(EarmarkedFlows.Input(80000, PLN(7000.0), PLN(1000000.0), 10, 15)),
      JstFlows.emit(JstFlows.Input(PLN(5000000.0), PLN(50000000.0), PLN(100000000.0), 9000, PLN(3000000.0))),
      HouseholdFlows.emit(
        HouseholdFlows
          .Input(PLN(40000000.0), PLN(8000000.0), PLN(5000000.0), PLN(3000000.0), PLN(1000000.0), PLN(500000.0), PLN(2000000.0), PLN(1500000.0), PLN(200000.0)),
      ),
      FirmFlows.emit(
        FirmFlows.Input(
          PLN(50000000.0),
          PLN(8000000.0),
          PLN(4000000.0),
          PLN(6000000.0),
          PLN(3000000.0),
          PLN(5000000.0),
          PLN(1000000.0),
          PLN(2000000.0),
          PLN(7000000.0),
          PLN(300000.0),
          PLN(800000.0),
          PLN(600000.0),
          PLN(5000000.0),
        ),
      ),
      GovBudgetFlows.emit(
        GovBudgetFlows.Input(PLN(15000000.0), PLN(10000000.0), PLN(4000000.0), PLN(2000000.0), PLN(3000000.0), PLN(1500000.0), PLN(2500000.0)),
      ),
      InsuranceFlows.emit(
        InsuranceFlows.Input(80000, PLN(7000.0), Share(0.08), PLN(100000000.0), PLN(50000000.0), PLN(40000000.0), Rate(0.06), Rate(0.08), Rate(0.03)),
      ),
      EquityFlows.emit(EquityFlows.Input(PLN(500000.0), PLN(200000.0), PLN(100000.0), PLN(1000000.0))),
      CorpBondFlows.emit(CorpBondFlows.Input(PLN(300000.0), PLN(50000.0), PLN(1000000.0), PLN(200000.0))),
      MortgageFlows.emit(MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0))),
      OpenEconFlows.emit(
        OpenEconFlows.Input(
          PLN(1000000.0),
          PLN(700000.0),
          PLN(250000.0),
          PLN(200000.0),
          PLN(300000.0),
          PLN(50000.0),
          PLN(-20000.0),
          PLN(400000.0),
          PLN(100000.0),
          PLN(150000.0),
        ),
      ),
      BankingFlows.emit(BankingFlows.Input(PLN(600000.0), PLN(200000.0), PLN(100000.0), PLN(-50000.0), PLN(80000.0), PLN(40000.0), PLN(30000.0), PLN(50000.0))),
    )

    val batchedFlows = Vector.concat(
      ZusFlows.emitBatches(ZusFlows.ZusInput(80000, PLN(7000.0), 1000)),
      NfzFlows.emitBatches(NfzFlows.NfzInput(80000, PLN(7000.0), 90000, 1000)),
      PpkFlows.emitBatches(PpkFlows.PpkInput(80000, PLN(7000.0))),
      EarmarkedFlows.emitBatches(EarmarkedFlows.Input(80000, PLN(7000.0), PLN(1000000.0), 10, 15)),
      JstFlows.emitBatches(JstFlows.Input(PLN(5000000.0), PLN(50000000.0), PLN(100000000.0), 9000, PLN(3000000.0))),
      HouseholdFlows.emitBatches(
        HouseholdFlows
          .Input(PLN(40000000.0), PLN(8000000.0), PLN(5000000.0), PLN(3000000.0), PLN(1000000.0), PLN(500000.0), PLN(2000000.0), PLN(1500000.0), PLN(200000.0)),
      ),
      FirmFlows.emitBatches(
        FirmFlows.Input(
          PLN(50000000.0),
          PLN(8000000.0),
          PLN(4000000.0),
          PLN(6000000.0),
          PLN(3000000.0),
          PLN(5000000.0),
          PLN(1000000.0),
          PLN(2000000.0),
          PLN(7000000.0),
          PLN(300000.0),
          PLN(800000.0),
          PLN(600000.0),
          PLN(5000000.0),
        ),
      ),
      GovBudgetFlows.emitBatches(
        GovBudgetFlows.Input(PLN(15000000.0), PLN(10000000.0), PLN(4000000.0), PLN(2000000.0), PLN(3000000.0), PLN(1500000.0), PLN(2500000.0)),
      ),
      InsuranceFlows.emitBatches(
        InsuranceFlows.Input(80000, PLN(7000.0), Share(0.08), PLN(100000000.0), PLN(50000000.0), PLN(40000000.0), Rate(0.06), Rate(0.08), Rate(0.03)),
      ),
      EquityFlows.emitBatches(EquityFlows.Input(PLN(500000.0), PLN(200000.0), PLN(100000.0), PLN(1000000.0))),
      CorpBondFlows.emitBatches(CorpBondFlows.Input(PLN(300000.0), PLN(50000.0), PLN(1000000.0), PLN(200000.0))),
      MortgageFlows.emitBatches(MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0))),
      OpenEconFlows.emitBatches(
        OpenEconFlows.Input(
          PLN(1000000.0),
          PLN(700000.0),
          PLN(250000.0),
          PLN(200000.0),
          PLN(300000.0),
          PLN(50000.0),
          PLN(-20000.0),
          PLN(400000.0),
          PLN(100000.0),
          PLN(150000.0),
        ),
      ),
      BankingFlows.emitBatches(
        BankingFlows.Input(PLN(600000.0), PLN(200000.0), PLN(100000.0), PLN(-50000.0), PLN(80000.0), PLN(40000.0), PLN(30000.0), PLN(50000.0)),
      ),
    )

    batchedMechanismTotals(batchedFlows) shouldBe legacyMechanismTotals(legacyFlows)
    batchedFlows.foreach(_ shouldBe a[BatchedFlow.Broadcast])
  }

  it should "drive FlowSimulation main path through BatchedFlow" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val legacy = AggregateBatchContract.toLegacyFlows(result.flows)

    result.flows should not be empty
    result.flows.forall(_.isInstanceOf[BatchedFlow]) shouldBe true
    batchedMechanismTotals(result.flows) shouldBe legacyMechanismTotals(legacy)
    result.execution.totalWealth shouldBe 0L
  }
