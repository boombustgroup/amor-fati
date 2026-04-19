package com.boombustgroup.amorfati.accounting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*

class InitCheckSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private def runtimeState(result: WorldInit.InitResult): Sfc.RuntimeState =
    val state = FlowSimulation.SimState.fromInit(result)
    Sfc.RuntimeState(state.world, state.firms, state.households, state.banks, state.ledgerFinancialState)

  private def stockSnapshot(result: WorldInit.InitResult): Sfc.StockState =
    Sfc.snapshot(runtimeState(result))

  "InitCheck" should "pass for default init" in:
    val result = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val errors = InitCheck.validate(runtimeState(result))
    errors shouldBe empty

  it should "have exact bond clearing at raw-unit precision for default init" in:
    val result   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snapshot = stockSnapshot(result)
    val holdings =
      snapshot.bankBondHoldings + snapshot.nbpBondHoldings + snapshot.foreignBondHoldings +
        snapshot.ppkBondHoldings + snapshot.insuranceGovBondHoldings + snapshot.tfiGovBondHoldings

    holdings shouldBe snapshot.bondsOutstanding

  it should "detect tampered bondsOutstanding" in:
    val result   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snap     = stockSnapshot(result)
    val tampered = snap.copy(bondsOutstanding = snap.bondsOutstanding + PLN(1000.0))
    val errors   = InitCheck.validate(tampered, result.banks, result.firms, result.households, result.ledgerFinancialState)
    errors should not be empty
    errors.exists(_.identity == "Bond clearing") shouldBe true

  it should "detect tampered bank deposits" in:
    val result         = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snap           = stockSnapshot(result)
    val bankBalances   = result.ledgerFinancialState.banks
    val tamperedLedger = result.ledgerFinancialState.copy(
      banks = bankBalances.updated(0, bankBalances(0).copy(totalDeposits = bankBalances(0).totalDeposits + PLN(5000.0))),
    )
    val errors         = InitCheck.validate(snap, result.banks, result.firms, result.households, tamperedLedger)
    errors should not be empty
    errors.exists(_.identity.startsWith("Deposit consistency")) shouldBe true

  it should "detect tampered firm debt" in:
    val result         = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snap           = stockSnapshot(result)
    val bankBalances   = result.ledgerFinancialState.banks
    val tamperedLedger = result.ledgerFinancialState.copy(
      banks = bankBalances.updated(0, bankBalances(0).copy(firmLoan = bankBalances(0).firmLoan + PLN(5000.0))),
    )
    val errors         = InitCheck.validate(snap, result.banks, result.firms, result.households, tamperedLedger)
    errors should not be empty
    errors.exists(_.identity.startsWith("Corp loan consistency")) shouldBe true

  it should "detect tampered consumer loans" in:
    val result         = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snap           = stockSnapshot(result)
    val bankBalances   = result.ledgerFinancialState.banks
    val tamperedLedger = result.ledgerFinancialState.copy(
      banks = bankBalances.updated(0, bankBalances(0).copy(consumerLoan = bankBalances(0).consumerLoan + PLN(5000.0))),
    )
    val errors         = InitCheck.validate(snap, result.banks, result.firms, result.households, tamperedLedger)
    errors should not be empty
    errors.exists(_.identity.startsWith("Consumer loan consistency")) shouldBe true
