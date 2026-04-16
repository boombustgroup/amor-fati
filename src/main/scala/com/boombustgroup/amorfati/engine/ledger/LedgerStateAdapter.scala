package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Banking, Firm, Household, Insurance, Nbfi, Nbp, QuasiFiscal}
import com.boombustgroup.amorfati.engine.{SocialState, World}
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, FiscalBudget}
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState.*

/** Narrow bridge between runtime simulation state and ledger-owned financial
  * storage.
  *
  * This adapter keeps only the current ledger-contracted financial slice
  * defined by [[AssetOwnershipContract]]: balances with a defensible mapping
  * onto existing ledger asset types. State such as physical capital, bank
  * capital, or mixed monthly operational flows is left out on purpose rather
  * than forced into the ledger abstraction under a misleading asset name.
  */
object LedgerStateAdapter:

  object FundIndex:
    val Zus: Int           = 0
    val Nfz: Int           = 1
    val Ppk: Int           = 2
    val Fp: Int            = 3
    val Pfron: Int         = 4
    val Fgsp: Int          = 5
    val Jst: Int           = 6
    val CorpBondOther: Int = 7
    val Nbfi: Int          = 8
    val QuasiFiscal: Int   = 9

    val Count: Int = 10

  case class GovernmentBondCircuit(
      outstanding: PLN,
      bankHoldings: PLN,
      foreignHoldings: PLN,
      nbpHoldings: PLN,
      insuranceHoldings: PLN,
      ppkHoldings: PLN,
      tfiHoldings: PLN,
  ):
    def totalHoldings: PLN =
      bankHoldings + foreignHoldings + nbpHoldings + insuranceHoldings + ppkHoldings + tfiHoldings

  case class CorporateBondCircuit(
      outstanding: PLN,
      bankHoldings: PLN,
      ppkHoldings: PLN,
      insuranceHoldings: PLN,
      tfiHoldings: PLN,
      otherHoldings: PLN,
  ):
    def totalHoldings: PLN =
      bankHoldings + ppkHoldings + insuranceHoldings + tfiHoldings + otherHoldings

  case class UnsupportedBankBalances(
      capital: PLN,
      nplAmount: PLN,
      consumerNpl: PLN,
      loansShort: PLN,
      loansMedium: PLN,
      loansLong: PLN,
  )

  case class UnsupportedGovernmentBalances(
      fiscalCumulativeDebt: PLN,
  )

  case class UnsupportedNbpBalances(
      qeCumulativePurchases: PLN,
  )

  case class UnsupportedQuasiFiscalBalances(
      bankHoldings: PLN,
      nbpHoldings: PLN,
  )

  case class UnsupportedJstBalances(
      jstDebt: PLN,
  )

  case class UnsupportedFinancialSnapshot(
      banks: Vector[UnsupportedBankBalances],
      government: UnsupportedGovernmentBalances,
      nbp: UnsupportedNbpBalances,
      jst: UnsupportedJstBalances,
      quasiFiscal: UnsupportedQuasiFiscalBalances,
  )

  def householdBalances(h: Household.State): HouseholdBalances =
    HouseholdBalances(
      demandDeposit = h.savings,
      mortgageLoan = h.debt,
      consumerLoan = h.consumerDebt,
      equity = h.equityWealth,
    )

  def firmBalances(f: Firm.State): FirmBalances =
    FirmBalances(
      cash = f.cash,
      firmLoan = f.debt,
      corpBond = f.bondDebt,
      equity = f.equityRaised,
    )

  def bankBalances(b: Banking.BankState): BankBalances =
    BankBalances(
      totalDeposits = b.deposits,
      demandDeposit = bankDemandDeposits(b),
      termDeposit = bankTermDeposits(b),
      firmLoan = b.loans,
      consumerLoan = b.consumerLoans,
      govBondAfs = b.afsBonds,
      govBondHtm = b.htmBonds,
      reserve = b.reservesAtNbp,
      interbankLoan = b.interbankNet,
      corpBond = b.corpBondHoldings,
    )

  def governmentBalances(gov: FiscalBudget.GovState): GovernmentBalances =
    GovernmentBalances(
      govBondOutstanding = gov.bondsOutstanding,
    )

  def foreignBalances(gov: FiscalBudget.GovState): ForeignBalances =
    ForeignBalances(
      govBondHoldings = gov.foreignBondHoldings,
    )

  def nbpBalances(nbp: Nbp.State): NbpBalances =
    NbpBalances(
      govBondHoldings = nbp.govBondHoldings,
      foreignAssets = nbp.fxReserves,
    )

  def insuranceBalances(insurance: Insurance.State): InsuranceBalances =
    InsuranceBalances(
      lifeReserve = insurance.lifeReserves,
      nonLifeReserve = insurance.nonLifeReserves,
      govBondHoldings = insurance.govBondHoldings,
      corpBondHoldings = insurance.corpBondHoldings,
      equityHoldings = insurance.equityHoldings,
    )

  def nbfiFundBalances(nbfi: Nbfi.State): NbfiFundBalances =
    NbfiFundBalances(
      tfiUnit = nbfi.tfiAum,
      govBondHoldings = nbfi.tfiGovBondHoldings,
      corpBondHoldings = nbfi.tfiCorpBondHoldings,
      equityHoldings = nbfi.tfiEquityHoldings,
      cashHoldings = nbfi.tfiCashHoldings,
      nbfiLoanStock = nbfi.nbfiLoanStock,
    )

  def quasiFiscalBalances(quasiFiscal: QuasiFiscal.State): QuasiFiscalBalances =
    QuasiFiscalBalances(
      bondsOutstanding = quasiFiscal.bondsOutstanding,
      loanPortfolio = quasiFiscal.loanPortfolio,
    )

  def fundBalances(
      social: SocialState,
      corporateBonds: CorporateBondMarket.State,
      nbfi: Nbfi.State,
      quasiFiscal: QuasiFiscal.State,
  ): FundBalances =
    FundBalances(
      zusCash = social.zus.fusBalance,
      nfzCash = social.nfz.balance,
      ppkGovBondHoldings = social.ppk.bondHoldings,
      ppkCorpBondHoldings = corporateBonds.ppkHoldings,
      fpCash = social.earmarked.fpBalance,
      pfronCash = social.earmarked.pfronBalance,
      fgspCash = social.earmarked.fgspBalance,
      jstCash = social.jst.deposits,
      corpBondOtherHoldings = corporateBonds.otherHoldings,
      nbfi = nbfiFundBalances(nbfi),
      quasiFiscal = quasiFiscalBalances(quasiFiscal),
    )

  def governmentBondCircuit(
      world: World,
      banks: Vector[Banking.BankState],
  ): GovernmentBondCircuit =
    val bankAgg = Banking.aggregateFromBanks(banks)
    GovernmentBondCircuit(
      outstanding = world.gov.bondsOutstanding,
      bankHoldings = bankAgg.govBondHoldings,
      foreignHoldings = world.gov.foreignBondHoldings,
      nbpHoldings = world.nbp.govBondHoldings,
      insuranceHoldings = world.financial.insurance.govBondHoldings,
      ppkHoldings = world.social.ppk.bondHoldings,
      tfiHoldings = world.financial.nbfi.tfiGovBondHoldings,
    )

  def governmentBondCircuit(
      ledgerFinancialState: LedgerFinancialState,
  ): GovernmentBondCircuit =
    val bankGovBondHoldings = ledgerFinancialState.banks.foldLeft(PLN.Zero)((acc, bank) => acc + bank.govBondAfs + bank.govBondHtm)
    GovernmentBondCircuit(
      outstanding = ledgerFinancialState.government.govBondOutstanding,
      bankHoldings = bankGovBondHoldings,
      foreignHoldings = ledgerFinancialState.foreign.govBondHoldings,
      nbpHoldings = ledgerFinancialState.nbp.govBondHoldings,
      insuranceHoldings = ledgerFinancialState.insurance.govBondHoldings,
      ppkHoldings = ledgerFinancialState.funds.ppkGovBondHoldings,
      tfiHoldings = ledgerFinancialState.funds.nbfi.govBondHoldings,
    )

  def corporateBondCircuit(
      ledgerFinancialState: LedgerFinancialState,
  ): CorporateBondCircuit =
    val bankCorpBondHoldings = ledgerFinancialState.banks.foldLeft(PLN.Zero)((acc, bank) => acc + bank.corpBond)
    CorporateBondCircuit(
      outstanding = PLN.fromRaw(ledgerFinancialState.firms.map(_.corpBond.toLong).sum),
      bankHoldings = bankCorpBondHoldings,
      ppkHoldings = ledgerFinancialState.funds.ppkCorpBondHoldings,
      insuranceHoldings = ledgerFinancialState.insurance.corpBondHoldings,
      tfiHoldings = ledgerFinancialState.funds.nbfi.corpBondHoldings,
      otherHoldings = ledgerFinancialState.funds.corpBondOtherHoldings,
    )

  def projectInsuranceState(
      base: Insurance.State,
      ledgerFinancialState: LedgerFinancialState,
  ): Insurance.State =
    base.copy(
      reserves = base.reserves.copy(
        lifeReserves = ledgerFinancialState.insurance.lifeReserve,
        nonLifeReserves = ledgerFinancialState.insurance.nonLifeReserve,
      ),
      portfolio = base.portfolio.copy(
        govBondHoldings = ledgerFinancialState.insurance.govBondHoldings,
        corpBondHoldings = ledgerFinancialState.insurance.corpBondHoldings,
        equityHoldings = ledgerFinancialState.insurance.equityHoldings,
      ),
    )

  def projectGovState(
      base: FiscalBudget.GovState,
      ledgerFinancialState: LedgerFinancialState,
  ): FiscalBudget.GovState =
    base.copy(
      financial = base.financial.copy(
        bondsOutstanding = ledgerFinancialState.government.govBondOutstanding,
        foreignBondHoldings = ledgerFinancialState.foreign.govBondHoldings,
      ),
    )

  def projectNbpState(
      base: Nbp.State,
      ledgerFinancialState: LedgerFinancialState,
  ): Nbp.State =
    base.copy(
      balance = base.balance.copy(
        govBondHoldings = ledgerFinancialState.nbp.govBondHoldings,
        fxReserves = ledgerFinancialState.nbp.foreignAssets,
      ),
    )

  def projectSocialState(
      base: SocialState,
      ledgerFinancialState: LedgerFinancialState,
  ): SocialState =
    base.copy(
      jst = base.jst.copy(deposits = ledgerFinancialState.funds.jstCash),
      zus = base.zus.copy(fusBalance = ledgerFinancialState.funds.zusCash),
      nfz = base.nfz.copy(balance = ledgerFinancialState.funds.nfzCash),
      ppk = base.ppk.copy(bondHoldings = ledgerFinancialState.funds.ppkGovBondHoldings),
      earmarked = base.earmarked.copy(
        fp = base.earmarked.fp.copy(balance = ledgerFinancialState.funds.fpCash),
        pfron = base.earmarked.pfron.copy(balance = ledgerFinancialState.funds.pfronCash),
        fgsp = base.earmarked.fgsp.copy(balance = ledgerFinancialState.funds.fgspCash),
      ),
    )

  def projectNbfiState(
      base: Nbfi.State,
      ledgerFinancialState: LedgerFinancialState,
  ): Nbfi.State =
    base.copy(
      tfi = base.tfi.copy(
        tfiAum = ledgerFinancialState.funds.nbfi.tfiUnit,
        tfiGovBondHoldings = ledgerFinancialState.funds.nbfi.govBondHoldings,
        tfiCorpBondHoldings = ledgerFinancialState.funds.nbfi.corpBondHoldings,
        tfiEquityHoldings = ledgerFinancialState.funds.nbfi.equityHoldings,
        tfiCashHoldings = ledgerFinancialState.funds.nbfi.cashHoldings,
      ),
      credit = base.credit.copy(
        nbfiLoanStock = ledgerFinancialState.funds.nbfi.nbfiLoanStock,
      ),
    )

  /** Legacy/bootstrap import from mirrored world/agent state into
    * `LedgerFinancialState`.
    *
    * This is the only intentional reverse direction. Normal monthly assembly
    * must carry `LedgerFinancialState` forward and project it into boundary
    * views, not rebuild it by round-tripping through world mirrors.
    */
  def captureLedgerFinancialState(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
  ): LedgerFinancialState =
    LedgerFinancialState(
      households = households.map(householdBalances),
      firms = firms.map(firmBalances),
      banks = banks.map(bankBalances),
      government = governmentBalances(world.gov),
      foreign = foreignBalances(world.gov),
      nbp = nbpBalances(world.nbp),
      insurance = insuranceBalances(world.financial.insurance),
      funds = fundBalances(world.social, world.financial.corporateBonds, world.financial.nbfi, world.financial.quasiFiscal),
    )

  /** Financial fields intentionally left outside current ledger mapping because
    * the public `EntitySector` / `AssetType` API does not yet name them
    * cleanly, or because they are fiscal/accounting metrics rather than
    * holder-tracked instruments.
    */
  def unsupportedSnapshot(sim: FlowSimulation.SimState): UnsupportedFinancialSnapshot =
    UnsupportedFinancialSnapshot(
      banks = sim.banks.map(b =>
        UnsupportedBankBalances(
          capital = b.capital,
          nplAmount = b.nplAmount,
          consumerNpl = b.consumerNpl,
          loansShort = b.loansShort,
          loansMedium = b.loansMedium,
          loansLong = b.loansLong,
        ),
      ),
      government = UnsupportedGovernmentBalances(
        fiscalCumulativeDebt = sim.world.gov.cumulativeDebt,
      ),
      nbp = UnsupportedNbpBalances(
        qeCumulativePurchases = sim.world.nbp.qeCumulative,
      ),
      jst = UnsupportedJstBalances(
        jstDebt = sim.world.social.jst.debt,
      ),
      quasiFiscal = UnsupportedQuasiFiscalBalances(
        bankHoldings = sim.world.financial.quasiFiscal.bankHoldings,
        nbpHoldings = sim.world.financial.quasiFiscal.nbpHoldings,
      ),
    )

  private def bankDemandDeposits(bank: Banking.BankState): PLN =
    if bank.demandDeposits == PLN.Zero && bank.termDeposits == PLN.Zero then bank.deposits
    else bank.demandDeposits

  private def bankTermDeposits(bank: Banking.BankState): PLN =
    if bank.demandDeposits == PLN.Zero && bank.termDeposits == PLN.Zero then PLN.Zero
    else bank.termDeposits
