package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.engine.SimulationMonth.CompletedMonth
import com.boombustgroup.amorfati.engine.ledger.LedgerStateAdapter
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.{ComputationBoundary, PLN}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorldAssemblyEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "WorldAssemblyEconomics (own Input)" should "produce valid world after simulation step" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val w      = result.nextState.world

    result.nextState.completedMonth shouldBe CompletedMonth(1)
    w.derivedTotalPopulation.should(be > 0)
    result.nextState.householdAggregates.employed.should(be > 0)
    w.external.tourismSeasonalFactor.should(not be 0.0)
  }

  it should "keep ETS observables at the base price in the first execution month" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val w      = result.nextState.world

    w.real.etsPrice.shouldBe(ComputationBoundary.toDouble(p.climate.etsBasePrice) +- 1e-10)
  }

  it should "preserve public-spending semantic aggregates on the assembled world" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val w      = result.nextState.world

    w.gov.domesticBudgetDemand shouldBe (w.gov.govCurrentSpend + w.gov.govCapitalSpend)
    w.gov.domesticBudgetOutlays shouldBe (
      w.gov.unempBenefitSpend
        + w.gov.socialTransferSpend
        + w.gov.govCurrentSpend
        + w.gov.govCapitalSpend
        + w.gov.debtServiceSpend
        + w.gov.euCofinancing
    )

    w.gov.domesticBudgetDemand shouldBe result.calculus.govPurchases
    w.gov.domesticBudgetOutlays.should(be >= w.gov.domesticBudgetDemand)
  }

  it should "overwrite only supported financial slice from ledger snapshot while preserving unsupported QE metrics" in {
    val init      = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val base      = init.world.copy(
      gov = init.world.gov.copy(
        financial = init.world.gov.financial.copy(
          cumulativeDebt = PLN(101),
          bondsOutstanding = PLN(102),
          foreignBondHoldings = PLN(103),
        ),
      ),
      nbp = init.world.nbp.copy(
        balance = init.world.nbp.balance.copy(
          govBondHoldings = PLN(104),
          qeCumulative = PLN(105),
          fxReserves = PLN(106),
        ),
      ),
      social = init.world.social.copy(
        jst = init.world.social.jst.copy(deposits = PLN(107), debt = PLN(108)),
        zus = init.world.social.zus.copy(fusBalance = PLN(109)),
        nfz = init.world.social.nfz.copy(balance = PLN(110)),
        ppk = init.world.social.ppk.copy(bondHoldings = PLN(111)),
        earmarked = init.world.social.earmarked.copy(
          fp = init.world.social.earmarked.fp.copy(balance = PLN(112)),
          pfron = init.world.social.earmarked.pfron.copy(balance = PLN(113)),
          fgsp = init.world.social.earmarked.fgsp.copy(balance = PLN(114)),
        ),
      ),
      financial = init.world.financial.copy(
        corporateBonds = init.world.financial.corporateBonds.copy(
          outstanding = PLN(115),
          bankHoldings = PLN(116),
          ppkHoldings = PLN(117),
          otherHoldings = PLN(118),
        ),
        insurance = init.world.financial.insurance.copy(
          reserves = init.world.financial.insurance.reserves.copy(
            lifeReserves = PLN(119),
            nonLifeReserves = PLN(120),
          ),
          portfolio = init.world.financial.insurance.portfolio.copy(
            govBondHoldings = PLN(121),
            corpBondHoldings = PLN(122),
            equityHoldings = PLN(123),
          ),
        ),
        nbfi = init.world.financial.nbfi.copy(
          tfi = init.world.financial.nbfi.tfi.copy(
            tfiAum = PLN(124),
            tfiGovBondHoldings = PLN(125),
            tfiCorpBondHoldings = PLN(126),
            tfiEquityHoldings = PLN(127),
            tfiCashHoldings = PLN(128),
          ),
          credit = init.world.financial.nbfi.credit.copy(
            nbfiLoanStock = PLN(129),
          ),
        ),
        quasiFiscal = init.world.financial.quasiFiscal.copy(
          bondsOutstanding = PLN(130),
          bankHoldings = PLN(131),
          nbpHoldings = PLN(132),
          loanPortfolio = PLN(133),
        ),
      ),
    )
    val supported = LedgerStateAdapter.SupportedFinancialSnapshot(
      households = Vector.empty,
      firms = Vector(
        LedgerStateAdapter.FirmBalances(
          cash = PLN.Zero,
          firmLoan = PLN.Zero,
          corpBond = PLN(301),
          equity = PLN.Zero,
        ),
        LedgerStateAdapter.FirmBalances(
          cash = PLN.Zero,
          firmLoan = PLN.Zero,
          corpBond = PLN(302),
          equity = PLN.Zero,
        ),
      ),
      banks = Vector(
        LedgerStateAdapter.BankBalances(
          totalDeposits = PLN.Zero,
          demandDeposit = PLN.Zero,
          termDeposit = PLN.Zero,
          firmLoan = PLN.Zero,
          consumerLoan = PLN.Zero,
          govBondAfs = PLN.Zero,
          govBondHtm = PLN.Zero,
          reserve = PLN.Zero,
          interbankLoan = PLN.Zero,
          corpBond = PLN(7),
        ),
        LedgerStateAdapter.BankBalances(
          totalDeposits = PLN.Zero,
          demandDeposit = PLN.Zero,
          termDeposit = PLN.Zero,
          firmLoan = PLN.Zero,
          consumerLoan = PLN.Zero,
          govBondAfs = PLN.Zero,
          govBondHtm = PLN.Zero,
          reserve = PLN.Zero,
          interbankLoan = PLN.Zero,
          corpBond = PLN(11),
        ),
      ),
      government = LedgerStateAdapter.GovernmentBalances(PLN(201)),
      foreign = LedgerStateAdapter.ForeignBalances(PLN(202)),
      nbp = LedgerStateAdapter.NbpBalances(
        govBondHoldings = PLN(203),
        foreignAssets = PLN(204),
      ),
      insurance = LedgerStateAdapter.InsuranceBalances(
        lifeReserve = PLN(205),
        nonLifeReserve = PLN(206),
        govBondHoldings = PLN(207),
        corpBondHoldings = PLN(208),
        equityHoldings = PLN(209),
      ),
      funds = LedgerStateAdapter.FundBalances(
        zusCash = PLN(210),
        nfzCash = PLN(211),
        ppkGovBondHoldings = PLN(212),
        ppkCorpBondHoldings = PLN(213),
        fpCash = PLN(214),
        pfronCash = PLN(215),
        fgspCash = PLN(216),
        jstCash = PLN(217),
        corpBondOtherHoldings = PLN(218),
        nbfi = LedgerStateAdapter.NbfiFundBalances(
          tfiUnit = PLN(219),
          govBondHoldings = PLN(220),
          corpBondHoldings = PLN(221),
          equityHoldings = PLN(222),
          cashHoldings = PLN(223),
          nbfiLoanStock = PLN(224),
        ),
        quasiFiscal = LedgerStateAdapter.QuasiFiscalBalances(
          bondsOutstanding = PLN(225),
          loanPortfolio = PLN(226),
        ),
      ),
    )

    val updated = WorldAssemblyEconomics.withLedgerSupportedFinancialState(base, supported)

    updated.gov.bondsOutstanding shouldBe PLN(201)
    updated.gov.foreignBondHoldings shouldBe PLN(202)
    updated.gov.cumulativeDebt shouldBe PLN(101)

    updated.nbp.govBondHoldings shouldBe PLN(203)
    updated.nbp.fxReserves shouldBe PLN(204)
    updated.nbp.qeCumulative shouldBe PLN(105)

    updated.social.jst.deposits shouldBe PLN(217)
    updated.social.zus.fusBalance shouldBe PLN(210)
    updated.social.nfz.balance shouldBe PLN(211)
    updated.social.ppk.bondHoldings shouldBe PLN(212)
    updated.social.earmarked.fpBalance shouldBe PLN(214)
    updated.social.earmarked.pfronBalance shouldBe PLN(215)
    updated.social.earmarked.fgspBalance shouldBe PLN(216)
    updated.social.jst.debt shouldBe PLN(108)

    updated.financial.corporateBonds.bankHoldings shouldBe PLN(18)
    updated.financial.corporateBonds.ppkHoldings shouldBe PLN(213)
    updated.financial.corporateBonds.outstanding shouldBe PLN(115)
    updated.financial.corporateBonds.otherHoldings shouldBe PLN(218)

    updated.financial.insurance.lifeReserves shouldBe PLN(205)
    updated.financial.insurance.nonLifeReserves shouldBe PLN(206)
    updated.financial.insurance.govBondHoldings shouldBe PLN(207)
    updated.financial.insurance.corpBondHoldings shouldBe PLN(208)
    updated.financial.insurance.equityHoldings shouldBe PLN(209)

    updated.financial.nbfi.tfiAum shouldBe PLN(219)
    updated.financial.nbfi.tfiGovBondHoldings shouldBe PLN(220)
    updated.financial.nbfi.tfiCorpBondHoldings shouldBe PLN(221)
    updated.financial.nbfi.tfiEquityHoldings shouldBe PLN(222)
    updated.financial.nbfi.tfiCashHoldings shouldBe PLN(223)
    updated.financial.nbfi.nbfiLoanStock shouldBe PLN(224)

    updated.financial.quasiFiscal.bondsOutstanding shouldBe PLN(225)
    updated.financial.quasiFiscal.loanPortfolio shouldBe PLN(226)
    updated.financial.quasiFiscal.bankHoldings shouldBe PLN(131)
    updated.financial.quasiFiscal.nbpHoldings shouldBe PLN(132)
  }
