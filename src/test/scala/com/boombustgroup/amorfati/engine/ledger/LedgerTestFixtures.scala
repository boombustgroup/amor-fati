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
          cumulativeDebt = PLN(776000000),
        ),
      ),
      nbp = base.world.nbp.copy(qeCumulative = PLN(89000000)),
      bop = base.world.bop.copy(
        nfa = PLN(91000000),
        foreignAssets = PLN(92000000),
        foreignLiabilities = PLN(93000000),
        reserves = PLN(94000000),
      ),
      social = base.world.social.copy(
        jst = Jst.State(
          debt = PLN(11000000),
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
        quasiFiscal = QuasiFiscal.State.zero,
      ),
    )

    val firms      = base.firms.updated(
      0,
      base.firms.head
        .copy(capitalStock = PLN(105000000)),
    )
    val firmStocks = base.ledgerFinancialState.firms
      .map(LedgerFinancialState.projectFirmFinancialStocks)
      .updated(0, Firm.FinancialStocks(cash = PLN(101000000), firmLoan = PLN(102000000), equity = PLN(104000000)))

    val households = base.households

    val banks      = base.banks.updated(
      0,
      base.banks.head
        .copy(
          capital = PLN(310000000),
          nplAmount = PLN(311000000),
          consumerNpl = PLN(312000000),
          loansShort = PLN(313000000),
          loansMedium = PLN(314000000),
          loansLong = PLN(315000000),
        ),
    )
    val bankStocks = base.ledgerFinancialState.banks
      .map(LedgerFinancialState.projectBankFinancialStocks)
      .updated(
        0,
        Banking.BankFinancialStocks(
          totalDeposits = PLN(603000000),
          demandDeposit = PLN(301000000),
          termDeposit = PLN(302000000),
          firmLoan = PLN(303000000),
          consumerLoan = PLN(304000000),
          govBondAfs = PLN(305000000),
          govBondHtm = PLN(306000000),
          reserve = PLN(307000000),
          interbankLoan = PLN(308000000),
        ),
      )

    val firmBalances = LedgerFinancialState
      .refreshFirmFinancialBalances(firmStocks, base.ledgerFinancialState.firms)
      .updated(0, LedgerFinancialState.firmBalances(firmStocks.head, corpBond = PLN(103000000)))
    val bankBalances = banks
      .zip(bankStocks)
      .map: (bank, stocks) =>
        LedgerFinancialState.bankBalances(stocks, corpBond = base.ledgerFinancialState.banks.lift(bank.id.toInt).fold(PLN.Zero)(_.corpBond))
      .updated(0, LedgerFinancialState.bankBalances(bankStocks.head, corpBond = PLN(309000000)))

    val ledgerFinancialState = base.ledgerFinancialState.copy(
      households = base.ledgerFinancialState.households.updated(
        0,
        LedgerFinancialState.HouseholdBalances(
          demandDeposit = PLN(201000000),
          mortgageLoan = PLN(202000000),
          consumerLoan = PLN(203000000),
          equity = PLN(204000000),
        ),
      ),
      firms = firmBalances,
      banks = bankBalances,
      government = LedgerFinancialState.GovernmentBalances(govBondOutstanding = PLN(777000000)),
      foreign = LedgerFinancialState.ForeignBalances(govBondHoldings = PLN(778000000)),
      nbp = LedgerFinancialState.NbpBalances(
        govBondHoldings = PLN(88000000),
        foreignAssets = PLN(99000000),
      ),
      funds = base.ledgerFinancialState.funds.copy(
        zusCash = PLN(11000000),
        nfzCash = PLN(12000000),
        ppkGovBondHoldings = PLN(13000000),
        fpCash = PLN(14000000),
        pfronCash = PLN(15000000),
        fgspCash = PLN(16000000),
        jstCash = PLN(10000000),
        quasiFiscal = LedgerFinancialState.quasiFiscalBalances(
          QuasiFiscal.StockState(
            bondsOutstanding = PLN(28000000),
            loanPortfolio = PLN(31000000),
            bankHoldings = PLN(29000000),
            nbpHoldings = PLN(30000000),
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
          lifeReserve = PLN(17000000),
          nonLifeReserve = PLN(18000000),
          govBondHoldings = PLN(19000000),
          corpBondHoldings = PLN(20000000),
          equityHoldings = PLN(21000000),
        ),
        funds = state.ledgerFinancialState.funds.copy(
          nbfi = LedgerFinancialState.NbfiFundBalances(
            tfiUnit = PLN(22000000),
            govBondHoldings = PLN(23000000),
            corpBondHoldings = PLN(24000000),
            equityHoldings = PLN(25000000),
            cashHoldings = PLN(26000000),
            nbfiLoanStock = PLN(27000000),
          ),
        ),
      ),
    )

end LedgerTestFixtures
