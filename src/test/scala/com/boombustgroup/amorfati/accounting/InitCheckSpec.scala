package com.boombustgroup.amorfati.accounting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*

class InitCheckSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "InitCheck" should "pass for default init" in:
    val result = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = Sfc.RuntimeState(
      result.world,
      result.firms,
      result.households,
      result.banks,
      LedgerFinancialState.bootstrapFromMirrors(result.world, result.firms, result.households, result.banks),
    )
    val errors = InitCheck.validate(state)
    errors shouldBe empty

  it should "have exact bond clearing at raw-unit precision for default init" in:
    val result   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snapshot = Sfc.snapshot(result.world, result.firms, result.households, result.banks)
    val holdings =
      snapshot.bankBondHoldings + snapshot.nbpBondHoldings + snapshot.foreignBondHoldings +
        snapshot.ppkBondHoldings + snapshot.insuranceGovBondHoldings + snapshot.tfiGovBondHoldings

    holdings shouldBe snapshot.bondsOutstanding

  it should "detect tampered bondsOutstanding" in:
    val result   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snap     = Sfc.snapshot(result.world, result.firms, result.households, result.banks)
    val tampered = snap.copy(bondsOutstanding = snap.bondsOutstanding + PLN(1000.0))
    val errors   = InitCheck.validate(tampered, result.banks, result.firms, result.households)
    errors should not be empty
    errors.exists(_.identity == "Bond clearing") shouldBe true

  it should "detect tampered bank deposits" in:
    val result        = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snap          = Sfc.snapshot(result.world, result.firms, result.households, result.banks)
    val banks         = result.banks
    val tamperedBank0 = banks.updated(0, banks(0).copy(deposits = banks(0).deposits + PLN(5000.0)))
    val errors        = InitCheck.validate(snap, tamperedBank0, result.firms, result.households)
    errors should not be empty
    errors.exists(_.identity.startsWith("Deposit consistency")) shouldBe true

  it should "detect tampered firm debt" in:
    val result        = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snap          = Sfc.snapshot(result.world, result.firms, result.households, result.banks)
    val banks         = result.banks
    val tamperedBank0 = banks.updated(0, banks(0).copy(loans = banks(0).loans + PLN(5000.0)))
    val errors        = InitCheck.validate(snap, tamperedBank0, result.firms, result.households)
    errors should not be empty
    errors.exists(_.identity.startsWith("Corp loan consistency")) shouldBe true

  it should "detect tampered consumer loans" in:
    val result        = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val snap          = Sfc.snapshot(result.world, result.firms, result.households, result.banks)
    val banks         = result.banks
    val tamperedBank0 = banks.updated(0, banks(0).copy(consumerLoans = banks(0).consumerLoans + PLN(5000.0)))
    val errors        = InitCheck.validate(snap, tamperedBank0, result.firms, result.households)
    errors should not be empty
    errors.exists(_.identity.startsWith("Consumer loan consistency")) shouldBe true
