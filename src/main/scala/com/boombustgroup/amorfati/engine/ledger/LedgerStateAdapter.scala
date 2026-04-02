package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, EntitySector, MutableWorldState}

/** Narrow bridge between runtime simulation state and ledger-owned financial
  * storage.
  *
  * This adapter intentionally supports only the current "clean" financial
  * slice: balances with a defensible mapping onto existing ledger
  * [[AssetType]]s. Unsupported state such as physical capital, bank capital, or
  * mixed monthly operational flows is left out on purpose rather than forced
  * into the ledger abstraction under a misleading asset name.
  */
object LedgerStateAdapter:

  object FundIndex:
    val Zus: Int         = 0
    val Nfz: Int         = 1
    val Ppk: Int         = 2
    val Fp: Int          = 3
    val Pfron: Int       = 4
    val Fgsp: Int        = 5
    val Nbfi: Int        = 6
    val QuasiFiscal: Int = 7

    val Count: Int = 8

  private val SingletonSectorSize = 1
  private val ForeignSectorSize   = 1

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
      fpCash: PLN,
      pfronCash: PLN,
      fgspCash: PLN,
      nbfi: NbfiFundBalances,
      quasiFiscal: QuasiFiscalBalances,
  )

  case class SupportedFinancialSnapshot(
      households: Vector[HouseholdBalances],
      firms: Vector[FirmBalances],
      banks: Vector[BankBalances],
      government: GovernmentBalances,
      nbp: NbpBalances,
      insurance: InsuranceBalances,
      funds: FundBalances,
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

  /** Pure supported-slice read from runtime state. */
  def supportedSnapshot(sim: FlowSimulation.SimState): SupportedFinancialSnapshot =
    SupportedFinancialSnapshot(
      households = sim.households.map(h =>
        HouseholdBalances(
          demandDeposit = h.savings,
          mortgageLoan = h.debt,
          consumerLoan = h.consumerDebt,
          equity = h.equityWealth,
        ),
      ),
      firms = sim.firms.map(f =>
        FirmBalances(
          cash = f.cash,
          firmLoan = f.debt,
          corpBond = f.bondDebt,
          equity = f.equityRaised,
        ),
      ),
      banks = sim.banks.map(b =>
        BankBalances(
          demandDeposit = b.demandDeposits,
          termDeposit = b.termDeposits,
          firmLoan = b.loans,
          consumerLoan = b.consumerLoans,
          govBondAfs = b.afsBonds,
          govBondHtm = b.htmBonds,
          reserve = b.reservesAtNbp,
          interbankLoan = b.interbankNet,
          corpBond = b.corpBondHoldings,
        ),
      ),
      government = GovernmentBalances(
        govBondOutstanding = sim.world.gov.bondsOutstanding,
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
        fpCash = sim.world.social.earmarked.fpBalance,
        pfronCash = sim.world.social.earmarked.pfronBalance,
        fgspCash = sim.world.social.earmarked.fgspBalance,
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

  def toMutableWorldState(sim: FlowSimulation.SimState): MutableWorldState =
    val state = new MutableWorldState(sectorSizes(sim))
    populate(state, sim)
    state

  def populate(state: MutableWorldState, sim: FlowSimulation.SimState): Unit =
    val supported = supportedSnapshot(sim)

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
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fp, supported.funds.fpCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Pfron, supported.funds.pfronCash)
    set(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fgsp, supported.funds.fgspCash)
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
        fpCash = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fp),
        pfronCash = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Pfron),
        fgspCash = pln(state, EntitySector.Funds, AssetType.Cash, FundIndex.Fgsp),
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
    state.setBalance(sector, asset, index, value.toLong) match
      case Right(_)  => ()
      case Left(err) =>
        throw new IllegalStateException(s"LedgerStateAdapter failed to populate ($sector, $asset, $index): $err")

  private def pln(state: MutableWorldState, sector: EntitySector, asset: AssetType, index: Int): PLN =
    PLN.fromRaw(state.balance(sector, asset, index))
