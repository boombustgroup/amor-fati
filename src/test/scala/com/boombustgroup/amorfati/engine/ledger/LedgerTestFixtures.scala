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
          cumulativeDebt = PLN(776e6),
        ),
      ),
      nbp = base.world.nbp.copy(qeCumulative = PLN(89e6)),
      bop = base.world.bop.copy(
        nfa = PLN(91e6),
        foreignAssets = PLN(92e6),
        foreignLiabilities = PLN(93e6),
        reserves = PLN(94e6),
      ),
      social = base.world.social.copy(
        jst = Jst.State(
          debt = PLN(11e6),
          revenue = PLN.Zero,
          spending = PLN.Zero,
          deficit = PLN.Zero,
        ),
        zus = SocialSecurity.ZusState.zero,
        nfz = SocialSecurity.NfzState.zero,
        ppk = SocialSecurity.PpkState(PLN.Zero),
        earmarked = EarmarkedFunds.State.zero,
      ),
      financialMarkets = base.world.financialMarkets.copy(
        quasiFiscal = QuasiFiscal.State(
          monthlyIssuance = PLN.Zero,
          monthlyLending = PLN.Zero,
        ),
      ),
    )

    val firms      = base.firms.updated(
      0,
      base.firms.head
        .copy(capitalStock = PLN(105e6)),
    )
    val firmStocks = base.ledgerFinancialState.firms
      .map(LedgerFinancialState.projectFirmFinancialStocks)
      .updated(0, Firm.FinancialStocks(cash = PLN(101e6), firmLoan = PLN(102e6), equity = PLN(104e6)))

    val households = base.households

    val banks      = base.banks.updated(
      0,
      base.banks.head
        .copy(
          capital = PLN(310e6),
          nplAmount = PLN(311e6),
          consumerNpl = PLN(312e6),
          loansShort = PLN(313e6),
          loansMedium = PLN(314e6),
          loansLong = PLN(315e6),
        ),
    )
    val bankStocks = base.ledgerFinancialState.banks
      .map(LedgerFinancialState.projectBankFinancialStocks)
      .updated(
        0,
        Banking.BankFinancialStocks(
          totalDeposits = PLN(603e6),
          demandDeposit = PLN(301e6),
          termDeposit = PLN(302e6),
          firmLoan = PLN(303e6),
          consumerLoan = PLN(304e6),
          govBondAfs = PLN(305e6),
          govBondHtm = PLN(306e6),
          reserve = PLN(307e6),
          interbankLoan = PLN(308e6),
        ),
      )

    val firmBalances = LedgerFinancialState
      .refreshFirmFinancialBalances(firmStocks, base.ledgerFinancialState.firms)
      .updated(0, LedgerFinancialState.firmBalances(firmStocks.head, corpBond = PLN(103e6)))
    val bankBalances = banks
      .zip(bankStocks)
      .map: (bank, stocks) =>
        LedgerFinancialState.bankBalances(stocks, corpBond = base.ledgerFinancialState.banks.lift(bank.id.toInt).fold(PLN.Zero)(_.corpBond))
      .updated(0, LedgerFinancialState.bankBalances(bankStocks.head, corpBond = PLN(309e6)))

    val ledgerFinancialState = base.ledgerFinancialState.copy(
      households = base.ledgerFinancialState.households.updated(
        0,
        LedgerFinancialState.HouseholdBalances(
          demandDeposit = PLN(201e6),
          mortgageLoan = PLN(202e6),
          consumerLoan = PLN(203e6),
          equity = PLN(204e6),
        ),
      ),
      firms = firmBalances,
      banks = bankBalances,
      government = LedgerFinancialState.GovernmentBalances(govBondOutstanding = PLN(777e6)),
      foreign = LedgerFinancialState.ForeignBalances(govBondHoldings = PLN(778e6)),
      nbp = LedgerFinancialState.NbpBalances(
        govBondHoldings = PLN(88e6),
        foreignAssets = PLN(99e6),
      ),
      funds = base.ledgerFinancialState.funds.copy(
        zusCash = PLN(11e6),
        nfzCash = PLN(12e6),
        ppkGovBondHoldings = PLN(13e6),
        fpCash = PLN(14e6),
        pfronCash = PLN(15e6),
        fgspCash = PLN(16e6),
        jstCash = PLN(10e6),
        quasiFiscal = LedgerFinancialState.quasiFiscalBalances(
          QuasiFiscal.StockState(
            bondsOutstanding = PLN(28e6),
            loanPortfolio = PLN(31e6),
            bankHoldings = PLN(29e6),
            nbpHoldings = PLN(30e6),
          ),
        ),
      ),
    )
    val state                = base.copy(
      world = world,
      firms = firms,
      households = households,
      banks = banks,
      ledgerFinancialState = ledgerFinancialState,
    )
    state.copy(
      ledgerFinancialState = state.ledgerFinancialState.copy(
        insurance = LedgerFinancialState.InsuranceBalances(
          lifeReserve = PLN(17e6),
          nonLifeReserve = PLN(18e6),
          govBondHoldings = PLN(19e6),
          corpBondHoldings = PLN(20e6),
          equityHoldings = PLN(21e6),
        ),
        funds = state.ledgerFinancialState.funds.copy(
          nbfi = LedgerFinancialState.NbfiFundBalances(
            tfiUnit = PLN(22e6),
            govBondHoldings = PLN(23e6),
            corpBondHoldings = PLN(24e6),
            equityHoldings = PLN(25e6),
            cashHoldings = PLN(26e6),
            nbfiLoanStock = PLN(27e6),
          ),
        ),
      ),
    )

end LedgerTestFixtures
