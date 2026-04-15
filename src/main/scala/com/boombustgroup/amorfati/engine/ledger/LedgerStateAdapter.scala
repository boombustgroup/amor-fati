package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Banking, Firm, Household, Insurance, Nbfi, Nbp}
import com.boombustgroup.amorfati.engine.{SocialState, World}
import com.boombustgroup.amorfati.engine.markets.FiscalBudget
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, EntitySector, MutableWorldState}
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState.*

/** Narrow bridge between runtime simulation state and ledger-owned financial
  * storage.
  *
  * This adapter intentionally supports only the current "clean" financial slice
  * defined by [[AssetOwnershipContract]]: balances with a defensible mapping
  * onto existing ledger [[AssetType]]s. Unsupported state such as physical
  * capital, bank capital, or mixed monthly operational flows is left out on
  * purpose rather than forced into the ledger abstraction under a misleading
  * asset name.
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

  private val SingletonSectorSize = 1
  private val ForeignSectorSize   = ForeignRuntimeContract.AllNodes.iterator.map(_.index).max + 1

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

  /** Deterministic ledger sector sizes for the current runtime state. */
  def sectorSizes(sim: FlowSimulation.SimState): Map[EntitySector, Int] =
    Map(
      EntitySector.Households -> sim.ledgerFinancialState.households.size,
      EntitySector.Firms      -> sim.ledgerFinancialState.firms.size,
      EntitySector.Banks      -> sim.ledgerFinancialState.banks.size,
      EntitySector.Government -> SingletonSectorSize,
      EntitySector.NBP        -> SingletonSectorSize,
      EntitySector.Insurance  -> SingletonSectorSize,
      EntitySector.Funds      -> FundIndex.Count,
      EntitySector.Foreign    -> ForeignSectorSize,
    )

  def sectorSizes(ledgerFinancialState: LedgerFinancialState): Map[EntitySector, Int] =
    Map(
      EntitySector.Households -> ledgerFinancialState.households.size,
      EntitySector.Firms      -> ledgerFinancialState.firms.size,
      EntitySector.Banks      -> ledgerFinancialState.banks.size,
      EntitySector.Government -> SingletonSectorSize,
      EntitySector.NBP        -> SingletonSectorSize,
      EntitySector.Insurance  -> SingletonSectorSize,
      EntitySector.Funds      -> FundIndex.Count,
      EntitySector.Foreign    -> ForeignSectorSize,
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
      world: World,
      firms: Vector[Firm.State],
      banks: Vector[Banking.BankState],
  ): CorporateBondCircuit =
    val bankAgg = Banking.aggregateFromBanks(banks)
    CorporateBondCircuit(
      outstanding = PLN.fromRaw(firms.map(_.bondDebt.toLong).sum),
      bankHoldings = bankAgg.corpBondHoldings,
      ppkHoldings = world.financial.corporateBonds.ppkHoldings,
      insuranceHoldings = world.financial.insurance.corpBondHoldings,
      tfiHoldings = world.financial.nbfi.tfiCorpBondHoldings,
      otherHoldings = world.financial.corporateBonds.otherHoldings,
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

  /** Transitional capture from mirrored world/agent state into the first-class
    * ledger-backed financial state.
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
      government = GovernmentBalances(
        govBondOutstanding = world.gov.bondsOutstanding,
      ),
      foreign = ForeignBalances(
        govBondHoldings = world.gov.foreignBondHoldings,
      ),
      nbp = NbpBalances(
        govBondHoldings = world.nbp.govBondHoldings,
        foreignAssets = world.nbp.fxReserves,
      ),
      insurance = InsuranceBalances(
        lifeReserve = world.financial.insurance.lifeReserves,
        nonLifeReserve = world.financial.insurance.nonLifeReserves,
        govBondHoldings = world.financial.insurance.govBondHoldings,
        corpBondHoldings = world.financial.insurance.corpBondHoldings,
        equityHoldings = world.financial.insurance.equityHoldings,
      ),
      funds = FundBalances(
        zusCash = world.social.zus.fusBalance,
        nfzCash = world.social.nfz.balance,
        ppkGovBondHoldings = world.social.ppk.bondHoldings,
        ppkCorpBondHoldings = world.financial.corporateBonds.ppkHoldings,
        fpCash = world.social.earmarked.fpBalance,
        pfronCash = world.social.earmarked.pfronBalance,
        fgspCash = world.social.earmarked.fgspBalance,
        jstCash = world.social.jst.deposits,
        corpBondOtherHoldings = world.financial.corporateBonds.otherHoldings,
        nbfi = NbfiFundBalances(
          tfiUnit = world.financial.nbfi.tfiAum,
          govBondHoldings = world.financial.nbfi.tfiGovBondHoldings,
          corpBondHoldings = world.financial.nbfi.tfiCorpBondHoldings,
          equityHoldings = world.financial.nbfi.tfiEquityHoldings,
          cashHoldings = world.financial.nbfi.tfiCashHoldings,
          nbfiLoanStock = world.financial.nbfi.nbfiLoanStock,
        ),
        quasiFiscal = QuasiFiscalBalances(
          bondsOutstanding = world.financial.quasiFiscal.bondsOutstanding,
          loanPortfolio = world.financial.quasiFiscal.loanPortfolio,
        ),
      ),
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

  def toMutableWorldState(sim: FlowSimulation.SimState): MutableWorldState =
    val state = new MutableWorldState(sectorSizes(sim))
    populate(state, sim)
    state

  def toMutableWorldState(ledgerFinancialState: LedgerFinancialState): MutableWorldState =
    val state = new MutableWorldState(sectorSizes(ledgerFinancialState))
    populate(state, ledgerFinancialState)
    state

  def roundTripLedgerFinancialState(ledgerFinancialState: LedgerFinancialState): LedgerFinancialState =
    readLedgerFinancialState(toMutableWorldState(ledgerFinancialState))

  def populate(state: MutableWorldState, sim: FlowSimulation.SimState): Unit =
    populate(state, sim.ledgerFinancialState)

  def populate(state: MutableWorldState, ledgerFinancialState: LedgerFinancialState): Unit =

    ledgerFinancialState.households.zipWithIndex.foreach { (hh, idx) =>
      set(state, EntitySector.Households, AssetType.DemandDeposit, idx, hh.demandDeposit)
      set(state, EntitySector.Households, AssetType.MortgageLoan, idx, hh.mortgageLoan)
      set(state, EntitySector.Households, AssetType.ConsumerLoan, idx, hh.consumerLoan)
      set(state, EntitySector.Households, AssetType.Equity, idx, hh.equity)
    }

    ledgerFinancialState.firms.zipWithIndex.foreach { (firm, idx) =>
      set(state, EntitySector.Firms, AssetType.Cash, idx, firm.cash)
      set(state, EntitySector.Firms, AssetType.FirmLoan, idx, firm.firmLoan)
      set(state, EntitySector.Firms, AssetType.CorpBond, idx, firm.corpBond)
      set(state, EntitySector.Firms, AssetType.Equity, idx, firm.equity)
    }

    ledgerFinancialState.banks.zipWithIndex.foreach { (bank, idx) =>
      set(state, EntitySector.Banks, AssetType.DemandDeposit, idx, bank.demandDeposit)
      set(state, EntitySector.Banks, AssetType.TermDeposit, idx, bank.termDeposit)
      set(state, EntitySector.Banks, AssetType.FirmLoan, idx, bank.firmLoan)
      set(state, EntitySector.Banks, AssetType.ConsumerLoan, idx, bank.consumerLoan)
      set(state, EntitySector.Banks, AssetType.GovBondAFS, idx, bank.govBondAfs)
      set(state, EntitySector.Banks, AssetType.GovBondHTM, idx, bank.govBondHtm)
      set(state, EntitySector.Banks, AssetType.Reserve, idx, bank.reserve)
      set(state, EntitySector.Banks, AssetType.InterbankLoan, idx, bank.interbankLoan)
      set(state, EntitySector.Banks, AssetType.CorpBond, idx, bank.corpBond)
    }

    set(state, EntitySector.Government, AssetType.GovBondHTM, 0, ledgerFinancialState.government.govBondOutstanding)
    set(state, EntitySector.Foreign, AssetType.GovBondHTM, 0, ledgerFinancialState.foreign.govBondHoldings)
    set(state, EntitySector.NBP, AssetType.GovBondHTM, 0, ledgerFinancialState.nbp.govBondHoldings)
    set(state, EntitySector.NBP, AssetType.ForeignAsset, 0, ledgerFinancialState.nbp.foreignAssets)
    set(state, EntitySector.Insurance, AssetType.LifeReserve, 0, ledgerFinancialState.insurance.lifeReserve)
    set(state, EntitySector.Insurance, AssetType.NonLifeReserve, 0, ledgerFinancialState.insurance.nonLifeReserve)
    set(state, EntitySector.Insurance, AssetType.GovBondHTM, 0, ledgerFinancialState.insurance.govBondHoldings)
    set(state, EntitySector.Insurance, AssetType.CorpBond, 0, ledgerFinancialState.insurance.corpBondHoldings)
    set(state, EntitySector.Insurance, AssetType.Equity, 0, ledgerFinancialState.insurance.equityHoldings)

    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Zus, ledgerFinancialState.funds.zusCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Nfz, ledgerFinancialState.funds.nfzCash)
    set(state, EntitySector.Funds, AssetType.GovBondHTM, FundIndex.Ppk, ledgerFinancialState.funds.ppkGovBondHoldings)
    set(state, EntitySector.Funds, AssetType.CorpBond, FundIndex.Ppk, ledgerFinancialState.funds.ppkCorpBondHoldings)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fp, ledgerFinancialState.funds.fpCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Pfron, ledgerFinancialState.funds.pfronCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fgsp, ledgerFinancialState.funds.fgspCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Jst, ledgerFinancialState.funds.jstCash)
    set(state, EntitySector.Funds, AssetType.CorpBond, FundIndex.CorpBondOther, ledgerFinancialState.funds.corpBondOtherHoldings)
    set(state, EntitySector.Funds, AssetType.TfiUnit, FundIndex.Nbfi, ledgerFinancialState.funds.nbfi.tfiUnit)
    set(state, EntitySector.Funds, AssetType.GovBondHTM, FundIndex.Nbfi, ledgerFinancialState.funds.nbfi.govBondHoldings)
    set(state, EntitySector.Funds, AssetType.CorpBond, FundIndex.Nbfi, ledgerFinancialState.funds.nbfi.corpBondHoldings)
    set(state, EntitySector.Funds, AssetType.Equity, FundIndex.Nbfi, ledgerFinancialState.funds.nbfi.equityHoldings)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Nbfi, ledgerFinancialState.funds.nbfi.cashHoldings)
    set(state, EntitySector.Funds, AssetType.NbfiLoan, FundIndex.Nbfi, ledgerFinancialState.funds.nbfi.nbfiLoanStock)
    set(
      state,
      EntitySector.Funds,
      AssetType.GovBondHTM,
      FundIndex.QuasiFiscal,
      ledgerFinancialState.funds.quasiFiscal.bondsOutstanding,
    )
    set(state, EntitySector.Funds, AssetType.NbfiLoan, FundIndex.QuasiFiscal, ledgerFinancialState.funds.quasiFiscal.loanPortfolio)

  def readLedgerFinancialState(state: MutableWorldState): LedgerFinancialState =
    LedgerFinancialState(
      households = Vector.tabulate(state.sectorSize(EntitySector.Households))(idx =>
        HouseholdBalances(
          demandDeposit = pln(state, EntitySector.Households, AssetType.DemandDeposit, idx),
          mortgageLoan = pln(state, EntitySector.Households, AssetType.MortgageLoan, idx),
          consumerLoan = pln(state, EntitySector.Households, AssetType.ConsumerLoan, idx),
          equity = pln(state, EntitySector.Households, AssetType.Equity, idx),
        ),
      ),
      firms = Vector.tabulate(state.sectorSize(EntitySector.Firms))(idx =>
        FirmBalances(
          cash = pln(state, EntitySector.Firms, AssetType.Cash, idx),
          firmLoan = pln(state, EntitySector.Firms, AssetType.FirmLoan, idx),
          corpBond = pln(state, EntitySector.Firms, AssetType.CorpBond, idx),
          equity = pln(state, EntitySector.Firms, AssetType.Equity, idx),
        ),
      ),
      banks = Vector.tabulate(state.sectorSize(EntitySector.Banks))(idx =>
        BankBalances(
          totalDeposits = pln(state, EntitySector.Banks, AssetType.DemandDeposit, idx) +
            pln(state, EntitySector.Banks, AssetType.TermDeposit, idx),
          demandDeposit = pln(state, EntitySector.Banks, AssetType.DemandDeposit, idx),
          termDeposit = pln(state, EntitySector.Banks, AssetType.TermDeposit, idx),
          firmLoan = pln(state, EntitySector.Banks, AssetType.FirmLoan, idx),
          consumerLoan = pln(state, EntitySector.Banks, AssetType.ConsumerLoan, idx),
          govBondAfs = pln(state, EntitySector.Banks, AssetType.GovBondAFS, idx),
          govBondHtm = pln(state, EntitySector.Banks, AssetType.GovBondHTM, idx),
          reserve = pln(state, EntitySector.Banks, AssetType.Reserve, idx),
          interbankLoan = pln(state, EntitySector.Banks, AssetType.InterbankLoan, idx),
          corpBond = pln(state, EntitySector.Banks, AssetType.CorpBond, idx),
        ),
      ),
      government = GovernmentBalances(
        govBondOutstanding = pln(state, EntitySector.Government, AssetType.GovBondHTM, 0),
      ),
      foreign = ForeignBalances(
        govBondHoldings = pln(state, EntitySector.Foreign, AssetType.GovBondHTM, 0),
      ),
      nbp = NbpBalances(
        govBondHoldings = pln(state, EntitySector.NBP, AssetType.GovBondHTM, 0),
        foreignAssets = pln(state, EntitySector.NBP, AssetType.ForeignAsset, 0),
      ),
      insurance = InsuranceBalances(
        lifeReserve = pln(state, EntitySector.Insurance, AssetType.LifeReserve, 0),
        nonLifeReserve = pln(state, EntitySector.Insurance, AssetType.NonLifeReserve, 0),
        govBondHoldings = pln(state, EntitySector.Insurance, AssetType.GovBondHTM, 0),
        corpBondHoldings = pln(state, EntitySector.Insurance, AssetType.CorpBond, 0),
        equityHoldings = pln(state, EntitySector.Insurance, AssetType.Equity, 0),
      ),
      funds = FundBalances(
        zusCash = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Zus),
        nfzCash = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Nfz),
        ppkGovBondHoldings = pln(state, EntitySector.Funds, AssetType.GovBondHTM, FundIndex.Ppk),
        ppkCorpBondHoldings = pln(state, EntitySector.Funds, AssetType.CorpBond, FundIndex.Ppk),
        fpCash = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fp),
        pfronCash = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Pfron),
        fgspCash = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fgsp),
        jstCash = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Jst),
        corpBondOtherHoldings = pln(state, EntitySector.Funds, AssetType.CorpBond, FundIndex.CorpBondOther),
        nbfi = NbfiFundBalances(
          tfiUnit = pln(state, EntitySector.Funds, AssetType.TfiUnit, FundIndex.Nbfi),
          govBondHoldings = pln(state, EntitySector.Funds, AssetType.GovBondHTM, FundIndex.Nbfi),
          corpBondHoldings = pln(state, EntitySector.Funds, AssetType.CorpBond, FundIndex.Nbfi),
          equityHoldings = pln(state, EntitySector.Funds, AssetType.Equity, FundIndex.Nbfi),
          cashHoldings = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Nbfi),
          nbfiLoanStock = pln(state, EntitySector.Funds, AssetType.NbfiLoan, FundIndex.Nbfi),
        ),
        quasiFiscal = QuasiFiscalBalances(
          bondsOutstanding = pln(state, EntitySector.Funds, AssetType.GovBondHTM, FundIndex.QuasiFiscal),
          loanPortfolio = pln(state, EntitySector.Funds, AssetType.NbfiLoan, FundIndex.QuasiFiscal),
        ),
      ),
    )

  private def set(state: MutableWorldState, sector: EntitySector, asset: AssetType, index: Int, value: PLN): Unit =
    AssetOwnershipContract.requireSupportedPersistedPair(sector, asset, index, "LedgerStateAdapter.set")
    state.setBalance(sector, asset, index, value.toLong) match
      case Right(_)  => ()
      case Left(err) =>
        throw new IllegalStateException(s"LedgerStateAdapter failed to populate ($sector, $asset, $index): $err")

  private def pln(state: MutableWorldState, sector: EntitySector, asset: AssetType, index: Int): PLN =
    AssetOwnershipContract.requireSupportedPersistedPair(sector, asset, index, "LedgerStateAdapter.pln")
    PLN.fromRaw(state.balance(sector, asset, index))

  private def bankDemandDeposits(bank: Banking.BankState): PLN =
    if bank.demandDeposits == PLN.Zero && bank.termDeposits == PLN.Zero then bank.deposits
    else bank.demandDeposits

  private def bankTermDeposits(bank: Banking.BankState): PLN =
    if bank.demandDeposits == PLN.Zero && bank.termDeposits == PLN.Zero then PLN.Zero
    else bank.termDeposits
