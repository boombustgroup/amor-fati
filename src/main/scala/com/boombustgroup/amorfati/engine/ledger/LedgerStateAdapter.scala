package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Banking, Firm, Household}
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.SimulationMonth.CompletedMonth
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, EntitySector, MutableWorldState}

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
  private val ForeignSectorSize   = ForeignRuntimeContract.TransferSettlement.index + 1

  case class HouseholdBalances(
      demandDeposit: PLN,
      mortgageLoan: PLN,
      consumerLoan: PLN,
      equity: PLN,
  )

  case class FirmBalances(
      cash: PLN,
      firmLoan: PLN,
      corpBond: PLN,
      equity: PLN,
  )

  case class BankBalances(
      totalDeposits: PLN,
      demandDeposit: PLN,
      termDeposit: PLN,
      firmLoan: PLN,
      consumerLoan: PLN,
      govBondAfs: PLN,
      govBondHtm: PLN,
      reserve: PLN,
      interbankLoan: PLN,
      corpBond: PLN,
  )

  case class GovernmentBalances(
      govBondOutstanding: PLN,
  )

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

  case class ForeignBalances(
      govBondHoldings: PLN,
  )

  case class NbpBalances(
      govBondHoldings: PLN,
      foreignAssets: PLN,
  )

  case class InsuranceBalances(
      lifeReserve: PLN,
      nonLifeReserve: PLN,
      govBondHoldings: PLN,
      corpBondHoldings: PLN,
      equityHoldings: PLN,
  )

  case class NbfiFundBalances(
      tfiUnit: PLN,
      govBondHoldings: PLN,
      corpBondHoldings: PLN,
      equityHoldings: PLN,
      cashHoldings: PLN,
      nbfiLoanStock: PLN,
  )

  case class QuasiFiscalBalances(
      bondsOutstanding: PLN,
      loanPortfolio: PLN,
  )

  case class FundBalances(
      zusCash: PLN,
      nfzCash: PLN,
      ppkGovBondHoldings: PLN,
      ppkCorpBondHoldings: PLN,
      fpCash: PLN,
      pfronCash: PLN,
      fgspCash: PLN,
      jstCash: PLN,
      corpBondOtherHoldings: PLN,
      nbfi: NbfiFundBalances,
      quasiFiscal: QuasiFiscalBalances,
  )

  case class SupportedFinancialSnapshot(
      households: Vector[HouseholdBalances],
      firms: Vector[FirmBalances],
      banks: Vector[BankBalances],
      government: GovernmentBalances,
      foreign: ForeignBalances,
      nbp: NbpBalances,
      insurance: InsuranceBalances,
      funds: FundBalances,
  )

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
      EntitySector.Households -> sim.households.size,
      EntitySector.Firms      -> sim.firms.size,
      EntitySector.Banks      -> sim.banks.size,
      EntitySector.Government -> SingletonSectorSize,
      EntitySector.NBP        -> SingletonSectorSize,
      EntitySector.Insurance  -> SingletonSectorSize,
      EntitySector.Funds      -> FundIndex.Count,
      EntitySector.Foreign    -> ForeignSectorSize,
    )

  def sectorSizes(supported: SupportedFinancialSnapshot): Map[EntitySector, Int] =
    Map(
      EntitySector.Households -> supported.households.size,
      EntitySector.Firms      -> supported.firms.size,
      EntitySector.Banks      -> supported.banks.size,
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
      supported: SupportedFinancialSnapshot,
  ): GovernmentBondCircuit =
    val bankGovBondHoldings = supported.banks.foldLeft(PLN.Zero)((acc, bank) => acc + bank.govBondAfs + bank.govBondHtm)
    GovernmentBondCircuit(
      outstanding = supported.government.govBondOutstanding,
      bankHoldings = bankGovBondHoldings,
      foreignHoldings = supported.foreign.govBondHoldings,
      nbpHoldings = supported.nbp.govBondHoldings,
      insuranceHoldings = supported.insurance.govBondHoldings,
      ppkHoldings = supported.funds.ppkGovBondHoldings,
      tfiHoldings = supported.funds.nbfi.govBondHoldings,
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
      supported: SupportedFinancialSnapshot,
  ): CorporateBondCircuit =
    val bankCorpBondHoldings = supported.banks.foldLeft(PLN.Zero)((acc, bank) => acc + bank.corpBond)
    CorporateBondCircuit(
      outstanding = PLN.fromRaw(supported.firms.map(_.corpBond.toLong).sum),
      bankHoldings = bankCorpBondHoldings,
      ppkHoldings = supported.funds.ppkCorpBondHoldings,
      insuranceHoldings = supported.insurance.corpBondHoldings,
      tfiHoldings = supported.funds.nbfi.corpBondHoldings,
      otherHoldings = supported.funds.corpBondOtherHoldings,
    )

  /** Pure supported-slice read from runtime state. */
  def supportedSnapshot(sim: FlowSimulation.SimState): SupportedFinancialSnapshot =
    SupportedFinancialSnapshot(
      households = sim.households.map(householdBalances),
      firms = sim.firms.map(firmBalances),
      banks = sim.banks.map(bankBalances),
      government = GovernmentBalances(
        govBondOutstanding = sim.world.gov.bondsOutstanding,
      ),
      foreign = ForeignBalances(
        govBondHoldings = sim.world.gov.foreignBondHoldings,
      ),
      nbp = NbpBalances(
        govBondHoldings = sim.world.nbp.govBondHoldings,
        foreignAssets = sim.world.nbp.fxReserves,
      ),
      insurance = InsuranceBalances(
        lifeReserve = sim.world.financial.insurance.lifeReserves,
        nonLifeReserve = sim.world.financial.insurance.nonLifeReserves,
        govBondHoldings = sim.world.financial.insurance.govBondHoldings,
        corpBondHoldings = sim.world.financial.insurance.corpBondHoldings,
        equityHoldings = sim.world.financial.insurance.equityHoldings,
      ),
      funds = FundBalances(
        zusCash = sim.world.social.zus.fusBalance,
        nfzCash = sim.world.social.nfz.balance,
        ppkGovBondHoldings = sim.world.social.ppk.bondHoldings,
        ppkCorpBondHoldings = sim.world.financial.corporateBonds.ppkHoldings,
        fpCash = sim.world.social.earmarked.fpBalance,
        pfronCash = sim.world.social.earmarked.pfronBalance,
        fgspCash = sim.world.social.earmarked.fgspBalance,
        jstCash = sim.world.social.jst.deposits,
        corpBondOtherHoldings = sim.world.financial.corporateBonds.otherHoldings,
        nbfi = NbfiFundBalances(
          tfiUnit = sim.world.financial.nbfi.tfiAum,
          govBondHoldings = sim.world.financial.nbfi.tfiGovBondHoldings,
          corpBondHoldings = sim.world.financial.nbfi.tfiCorpBondHoldings,
          equityHoldings = sim.world.financial.nbfi.tfiEquityHoldings,
          cashHoldings = sim.world.financial.nbfi.tfiCashHoldings,
          nbfiLoanStock = sim.world.financial.nbfi.nbfiLoanStock,
        ),
        quasiFiscal = QuasiFiscalBalances(
          bondsOutstanding = sim.world.financial.quasiFiscal.bondsOutstanding,
          loanPortfolio = sim.world.financial.quasiFiscal.loanPortfolio,
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

  def toMutableWorldState(supported: SupportedFinancialSnapshot): MutableWorldState =
    val state = new MutableWorldState(sectorSizes(supported))
    populate(state, supported)
    state

  def roundTripSupported(sim: FlowSimulation.SimState): SupportedFinancialSnapshot =
    readSupported(toMutableWorldState(sim))

  def roundTripSupported(supported: SupportedFinancialSnapshot): SupportedFinancialSnapshot =
    readSupported(toMutableWorldState(supported))

  def roundTripSupported(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
  ): SupportedFinancialSnapshot =
    roundTripSupported(FlowSimulation.SimState(CompletedMonth.Zero, world, firms, households, banks, householdAggregates))

  def populate(state: MutableWorldState, sim: FlowSimulation.SimState): Unit =
    val supported = supportedSnapshot(sim)
    populate(state, supported)

  def populate(state: MutableWorldState, supported: SupportedFinancialSnapshot): Unit =

    supported.households.zipWithIndex.foreach { (hh, idx) =>
      set(state, EntitySector.Households, AssetType.DemandDeposit, idx, hh.demandDeposit)
      set(state, EntitySector.Households, AssetType.MortgageLoan, idx, hh.mortgageLoan)
      set(state, EntitySector.Households, AssetType.ConsumerLoan, idx, hh.consumerLoan)
      set(state, EntitySector.Households, AssetType.Equity, idx, hh.equity)
    }

    supported.firms.zipWithIndex.foreach { (firm, idx) =>
      set(state, EntitySector.Firms, AssetType.Cash, idx, firm.cash)
      set(state, EntitySector.Firms, AssetType.FirmLoan, idx, firm.firmLoan)
      set(state, EntitySector.Firms, AssetType.CorpBond, idx, firm.corpBond)
      set(state, EntitySector.Firms, AssetType.Equity, idx, firm.equity)
    }

    supported.banks.zipWithIndex.foreach { (bank, idx) =>
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

    set(state, EntitySector.Government, AssetType.GovBondHTM, 0, supported.government.govBondOutstanding)
    set(state, EntitySector.Foreign, AssetType.GovBondHTM, 0, supported.foreign.govBondHoldings)
    set(state, EntitySector.NBP, AssetType.GovBondHTM, 0, supported.nbp.govBondHoldings)
    set(state, EntitySector.NBP, AssetType.ForeignAsset, 0, supported.nbp.foreignAssets)
    set(state, EntitySector.Insurance, AssetType.LifeReserve, 0, supported.insurance.lifeReserve)
    set(state, EntitySector.Insurance, AssetType.NonLifeReserve, 0, supported.insurance.nonLifeReserve)
    set(state, EntitySector.Insurance, AssetType.GovBondHTM, 0, supported.insurance.govBondHoldings)
    set(state, EntitySector.Insurance, AssetType.CorpBond, 0, supported.insurance.corpBondHoldings)
    set(state, EntitySector.Insurance, AssetType.Equity, 0, supported.insurance.equityHoldings)

    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Zus, supported.funds.zusCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Nfz, supported.funds.nfzCash)
    set(state, EntitySector.Funds, AssetType.GovBondHTM, FundIndex.Ppk, supported.funds.ppkGovBondHoldings)
    set(state, EntitySector.Funds, AssetType.CorpBond, FundIndex.Ppk, supported.funds.ppkCorpBondHoldings)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fp, supported.funds.fpCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Pfron, supported.funds.pfronCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fgsp, supported.funds.fgspCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Jst, supported.funds.jstCash)
    set(state, EntitySector.Funds, AssetType.CorpBond, FundIndex.CorpBondOther, supported.funds.corpBondOtherHoldings)
    set(state, EntitySector.Funds, AssetType.TfiUnit, FundIndex.Nbfi, supported.funds.nbfi.tfiUnit)
    set(state, EntitySector.Funds, AssetType.GovBondHTM, FundIndex.Nbfi, supported.funds.nbfi.govBondHoldings)
    set(state, EntitySector.Funds, AssetType.CorpBond, FundIndex.Nbfi, supported.funds.nbfi.corpBondHoldings)
    set(state, EntitySector.Funds, AssetType.Equity, FundIndex.Nbfi, supported.funds.nbfi.equityHoldings)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Nbfi, supported.funds.nbfi.cashHoldings)
    set(state, EntitySector.Funds, AssetType.NbfiLoan, FundIndex.Nbfi, supported.funds.nbfi.nbfiLoanStock)
    set(state, EntitySector.Funds, AssetType.GovBondHTM, FundIndex.QuasiFiscal, supported.funds.quasiFiscal.bondsOutstanding)
    set(state, EntitySector.Funds, AssetType.NbfiLoan, FundIndex.QuasiFiscal, supported.funds.quasiFiscal.loanPortfolio)

  def readSupported(state: MutableWorldState): SupportedFinancialSnapshot =
    SupportedFinancialSnapshot(
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
