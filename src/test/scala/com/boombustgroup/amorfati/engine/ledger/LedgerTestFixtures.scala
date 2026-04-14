package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*

object LedgerTestFixtures:

  private given SimParams = SimParams.defaults

  def simState(seed: Long = 42L): FlowSimulation.SimState =
    val init = WorldInit.initialize(InitRandomness.Contract.fromSeed(seed))
    FlowSimulation.SimState.fromInit(init)

  def enrichedSimState(): FlowSimulation.SimState =
    val base  = simState()
    val world = base.world.copy(
      gov = base.world.gov.copy(
        financial = base.world.gov.financial.copy(
          bondsOutstanding = PLN(777e6),
          foreignBondHoldings = PLN(778e6),
        ),
      ),
      nbp = base.world.nbp.copy(
        balance = base.world.nbp.balance.copy(
          govBondHoldings = PLN(88e6),
          qeCumulative = PLN(89e6),
          fxReserves = PLN(99e6),
        ),
      ),
      social = base.world.social.copy(
        jst = Jst.State(
          deposits = PLN(10e6),
          debt = PLN(11e6),
          revenue = PLN.Zero,
          spending = PLN.Zero,
          deficit = PLN.Zero,
        ),
        zus = SocialSecurity.ZusState(PLN(11e6), PLN.Zero, PLN.Zero, PLN.Zero),
        nfz = SocialSecurity.NfzState(PLN(12e6), PLN.Zero, PLN.Zero, PLN.Zero),
        ppk = SocialSecurity.PpkState(PLN(13e6), PLN.Zero),
        earmarked = EarmarkedFunds.State(
          fpBalance = PLN(14e6),
          fpContributions = PLN.Zero,
          fpSpending = PLN.Zero,
          pfronBalance = PLN(15e6),
          pfronContributions = PLN.Zero,
          pfronSpending = PLN.Zero,
          fgspBalance = PLN(16e6),
          fgspContributions = PLN.Zero,
          fgspSpending = PLN.Zero,
          totalGovSubvention = PLN.Zero,
        ),
      ),
      financial = base.world.financial.copy(
        corporateBonds = base.world.financial.corporateBonds.copy(
          outstanding = PLN(32e6),
          ppkHoldings = PLN(33e6),
          otherHoldings = PLN(34e6),
        ),
        insurance = Insurance.State(
          lifeReserves = PLN(17e6),
          nonLifeReserves = PLN(18e6),
          govBondHoldings = PLN(19e6),
          corpBondHoldings = PLN(20e6),
          equityHoldings = PLN(21e6),
          lastLifePremium = PLN.Zero,
          lastNonLifePremium = PLN.Zero,
          lastLifeClaims = PLN.Zero,
          lastNonLifeClaims = PLN.Zero,
          lastInvestmentIncome = PLN.Zero,
          lastNetDepositChange = PLN.Zero,
        ),
        nbfi = Nbfi.State(
          tfiAum = PLN(22e6),
          tfiGovBondHoldings = PLN(23e6),
          tfiCorpBondHoldings = PLN(24e6),
          tfiEquityHoldings = PLN(25e6),
          tfiCashHoldings = PLN(26e6),
          nbfiLoanStock = PLN(27e6),
          lastTfiNetInflow = PLN.Zero,
          lastNbfiOrigination = PLN.Zero,
          lastNbfiRepayment = PLN.Zero,
          lastNbfiDefaultAmount = PLN.Zero,
          lastNbfiInterestIncome = PLN.Zero,
          lastBankTightness = Share.Zero,
          lastDepositDrain = PLN.Zero,
        ),
        quasiFiscal = QuasiFiscal.State(
          bondsOutstanding = PLN(28e6),
          bankHoldings = PLN(29e6),
          nbpHoldings = PLN(30e6),
          loanPortfolio = PLN(31e6),
          monthlyIssuance = PLN.Zero,
          monthlyLending = PLN.Zero,
        ),
      ),
    )

    val firms = base.firms.updated(
      0,
      base.firms.head.copy(
        cash = PLN(101e6),
        debt = PLN(102e6),
        bondDebt = PLN(103e6),
        equityRaised = PLN(104e6),
        capitalStock = PLN(105e6),
      ),
    )

    val households = base.households.updated(
      0,
      base.households.head.copy(
        savings = PLN(201e6),
        debt = PLN(202e6),
        consumerDebt = PLN(203e6),
        equityWealth = PLN(204e6),
      ),
    )

    val banks = base.banks.updated(
      0,
      base.banks.head.copy(
        deposits = PLN(603e6),
        demandDeposits = PLN(301e6),
        termDeposits = PLN(302e6),
        loans = PLN(303e6),
        consumerLoans = PLN(304e6),
        afsBonds = PLN(305e6),
        htmBonds = PLN(306e6),
        reservesAtNbp = PLN(307e6),
        interbankNet = PLN(308e6),
        corpBondHoldings = PLN(309e6),
        capital = PLN(310e6),
        nplAmount = PLN(311e6),
        consumerNpl = PLN(312e6),
        loansShort = PLN(313e6),
        loansMedium = PLN(314e6),
        loansLong = PLN(315e6),
      ),
    )

    base.copy(world = world, firms = firms, households = households, banks = banks)

end LedgerTestFixtures
