package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.{FlowSimulation, RuntimeLedgerTopology}
import com.boombustgroup.amorfati.types.PLN
import com.boombustgroup.ledger.{AssetType, EntitySector}

/** Engine-side ownership contract for the current ledger-backed financial
  * slice.
  *
  * This is intentionally narrower than the public `amor-fati-ledger` API. It
  * states which `(EntitySector, AssetType)` pairs are currently treated as
  * supported persisted stock by the engine, which persisted families are
  * intentionally unsupported, and which aggregate runtime nodes are execution /
  * settlement shells rather than persisted owners.
  */
object AssetOwnershipContract:

  private val zeroPopulationTopology = RuntimeLedgerTopology.zeroPopulation

  enum SectorId:
    case Dynamic(sector: EntitySector)
    case Fixed(sector: EntitySector, index: Int)

    def entitySector: EntitySector =
      this match
        case Dynamic(sector)  => sector
        case Fixed(sector, _) => sector

    def matches(
        sector: EntitySector,
        index: Int,
    ): Boolean =
      this match
        case Dynamic(ownerSector)         => ownerSector == sector
        case Fixed(ownerSector, ownerIdx) => ownerSector == sector && ownerIdx == index

  case class SupportedPair(
      owner: SectorId,
      asset: AssetType,
  ):
    def matches(
        sector: EntitySector,
        index: Int,
    ): Boolean =
      owner.matches(sector, index)

  enum PublicAssetStatus:
    case SupportedPersistedStock
    case PublicAssetWithoutEngineContract

  case class PublicAssetContract(
      asset: AssetType,
      status: PublicAssetStatus,
      supportedSlots: Set[SectorId],
      note: String,
  )

  private def dynamic(sector: EntitySector): SectorId =
    SectorId.Dynamic(sector)

  private def singleton(sector: EntitySector): SectorId =
    SectorId.Fixed(sector, 0)

  private def fund(index: Int): SectorId =
    SectorId.Fixed(EntitySector.Funds, index)

  val publicAssets: Vector[PublicAssetContract] = Vector(
    PublicAssetContract(
      AssetType.DemandDeposit,
      PublicAssetStatus.SupportedPersistedStock,
      Set(dynamic(EntitySector.Households), dynamic(EntitySector.Banks)),
      "Household savings and bank demand-deposit liabilities.",
    ),
    PublicAssetContract(
      AssetType.TermDeposit,
      PublicAssetStatus.SupportedPersistedStock,
      Set(dynamic(EntitySector.Banks)),
      "Bank term-deposit liabilities.",
    ),
    PublicAssetContract(
      AssetType.FirmLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(dynamic(EntitySector.Firms), dynamic(EntitySector.Banks)),
      "Firm debt mirrored by bank loan assets.",
    ),
    PublicAssetContract(
      AssetType.ConsumerLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(dynamic(EntitySector.Households), dynamic(EntitySector.Banks)),
      "Household consumer debt mirrored by bank loan assets.",
    ),
    PublicAssetContract(
      AssetType.MortgageLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(dynamic(EntitySector.Households)),
      "Currently only the household-side mortgage liability is in the supported slice.",
    ),
    PublicAssetContract(
      AssetType.GovBondAFS,
      PublicAssetStatus.SupportedPersistedStock,
      Set(dynamic(EntitySector.Banks)),
      "Bank-only AFS government-bond holdings.",
    ),
    PublicAssetContract(
      AssetType.GovBondHTM,
      PublicAssetStatus.SupportedPersistedStock,
      Set(
        dynamic(EntitySector.Banks),
        singleton(EntitySector.Government),
        singleton(EntitySector.Foreign),
        singleton(EntitySector.NBP),
        singleton(EntitySector.Insurance),
        fund(LedgerStateAdapter.FundIndex.Ppk),
        fund(LedgerStateAdapter.FundIndex.Nbfi),
        fund(LedgerStateAdapter.FundIndex.QuasiFiscal),
      ),
      "Issuer outstanding plus holder stocks across supported sectors.",
    ),
    PublicAssetContract(
      AssetType.CorpBond,
      PublicAssetStatus.SupportedPersistedStock,
      Set(
        dynamic(EntitySector.Firms),
        dynamic(EntitySector.Banks),
        singleton(EntitySector.Insurance),
        fund(LedgerStateAdapter.FundIndex.Ppk),
        fund(LedgerStateAdapter.FundIndex.CorpBondOther),
        fund(LedgerStateAdapter.FundIndex.Nbfi),
      ),
      "Firm bond liabilities plus supported holder stocks.",
    ),
    PublicAssetContract(
      AssetType.Reserve,
      PublicAssetStatus.SupportedPersistedStock,
      Set(dynamic(EntitySector.Banks)),
      "Bank reserve asset; the NBP-side reserve settlement liability remains a delta-only runtime shell.",
    ),
    PublicAssetContract(
      AssetType.StandingFacility,
      PublicAssetStatus.PublicAssetWithoutEngineContract,
      Set.empty,
      "The engine uses standing-facility mechanisms as a delta-only NBP backstop channel, but does not persist this as a stock pair.",
    ),
    PublicAssetContract(
      AssetType.InterbankLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(dynamic(EntitySector.Banks)),
      "Aggregate net interbank position per bank.",
    ),
    PublicAssetContract(
      AssetType.Equity,
      PublicAssetStatus.SupportedPersistedStock,
      Set(
        dynamic(EntitySector.Households),
        dynamic(EntitySector.Firms),
        singleton(EntitySector.Insurance),
        fund(LedgerStateAdapter.FundIndex.Nbfi),
      ),
      "Supported stock family despite separate timing-cleanup work for equity flows.",
    ),
    PublicAssetContract(
      AssetType.LifeReserve,
      PublicAssetStatus.SupportedPersistedStock,
      Set(singleton(EntitySector.Insurance)),
      "Insurance-side life reserve stock.",
    ),
    PublicAssetContract(
      AssetType.NonLifeReserve,
      PublicAssetStatus.SupportedPersistedStock,
      Set(singleton(EntitySector.Insurance)),
      "Insurance-side non-life reserve stock.",
    ),
    PublicAssetContract(
      AssetType.TfiUnit,
      PublicAssetStatus.SupportedPersistedStock,
      Set(fund(LedgerStateAdapter.FundIndex.Nbfi)),
      "TFI AUM proxy inside the NBFI fund bucket.",
    ),
    PublicAssetContract(
      AssetType.NbfiLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(
        fund(LedgerStateAdapter.FundIndex.Nbfi),
        fund(LedgerStateAdapter.FundIndex.QuasiFiscal),
      ),
      "Used for both NBFI credit and quasi-fiscal loan portfolio inside the Funds sector.",
    ),
    PublicAssetContract(
      AssetType.Cash,
      PublicAssetStatus.SupportedPersistedStock,
      Set(
        dynamic(EntitySector.Firms),
        fund(LedgerStateAdapter.FundIndex.Zus),
        fund(LedgerStateAdapter.FundIndex.Nfz),
        fund(LedgerStateAdapter.FundIndex.Fp),
        fund(LedgerStateAdapter.FundIndex.Pfron),
        fund(LedgerStateAdapter.FundIndex.Fgsp),
        fund(LedgerStateAdapter.FundIndex.Jst),
        fund(LedgerStateAdapter.FundIndex.Nbfi),
      ),
      "Supported only for firm cash and selected fund buckets; not a universal settlement cash asset.",
    ),
    PublicAssetContract(
      AssetType.Capital,
      PublicAssetStatus.PublicAssetWithoutEngineContract,
      Set.empty,
      "Physical capital exists in the engine, but not as a supported ledger-owned stock family.",
    ),
    PublicAssetContract(
      AssetType.ForeignAsset,
      PublicAssetStatus.SupportedPersistedStock,
      Set(singleton(EntitySector.NBP)),
      "NBP FX reserve stock.",
    ),
  )

  private val publicAssetGroups = publicAssets.groupBy(_.asset)

  require(
    publicAssetGroups.keySet == AssetType.values.toSet,
    "AssetOwnershipContract must classify every public AssetType.",
  )

  require(
    publicAssetGroups.values.forall(_.size == 1),
    "AssetOwnershipContract must define exactly one PublicAssetContract per AssetType.",
  )

  private val publicAssetsByAsset =
    publicAssetGroups.view.mapValues(_.head).toMap

  val supportedPairs: Set[SupportedPair] =
    publicAssetsByAsset.valuesIterator
      .filter(_.status == PublicAssetStatus.SupportedPersistedStock)
      .flatMap: contract =>
        contract.supportedSlots.iterator.map(slot => SupportedPair(slot, contract.asset))
      .toSet

  val orphanPublicAssets: Set[AssetType] =
    publicAssets.iterator
      .filter(_.status == PublicAssetStatus.PublicAssetWithoutEngineContract)
      .map(_.asset)
      .toSet

  enum UnsupportedCategory:
    case UnsupportedPersistedStock
    case MetricOnly

  enum UnsupportedFamilyId:
    case BankCapital
    case BankCreditRiskState
    case GovernmentFiscalCumulativeDebt
    case NbpQeCumulativePurchases
    case JstDebt
    case QuasiFiscalHolderSplits

  case class UnsupportedFamily(
      id: UnsupportedFamilyId,
      category: UnsupportedCategory,
      note: String,
  )

  val unsupportedFamilies: Vector[UnsupportedFamily] = Vector(
    UnsupportedFamily(
      UnsupportedFamilyId.BankCapital,
      UnsupportedCategory.UnsupportedPersistedStock,
      "Bank capital is persisted, but not modeled as a supported transferable asset family.",
    ),
    UnsupportedFamily(
      UnsupportedFamilyId.BankCreditRiskState,
      UnsupportedCategory.UnsupportedPersistedStock,
      "NPL and loan-maturity buckets are accounting/risk state rather than holder-tracked instruments.",
    ),
    UnsupportedFamily(
      UnsupportedFamilyId.GovernmentFiscalCumulativeDebt,
      UnsupportedCategory.MetricOnly,
      "Cumulative fiscal debt is a policy/accounting metric rather than treasury settlement stock.",
    ),
    UnsupportedFamily(
      UnsupportedFamilyId.NbpQeCumulativePurchases,
      UnsupportedCategory.MetricOnly,
      "QE cumulative purchases are tracked as a policy metric rather than a plain supported asset stock.",
    ),
    UnsupportedFamily(
      UnsupportedFamilyId.JstDebt,
      UnsupportedCategory.UnsupportedPersistedStock,
      "JST debt persists in the world, but lacks a holder-resolved supported asset contract.",
    ),
    UnsupportedFamily(
      UnsupportedFamilyId.QuasiFiscalHolderSplits,
      UnsupportedCategory.UnsupportedPersistedStock,
      "Quasi-fiscal bank/NBP holder splits persist, but are not yet supported in the ledger-owned slice.",
    ),
  )

  def presentUnsupportedFamilies(sim: FlowSimulation.SimState): Set[UnsupportedFamilyId] =
    Set.empty[UnsupportedFamilyId]
      ++ Option.when(sim.banks.exists(_.capital != PLN.Zero))(UnsupportedFamilyId.BankCapital)
      ++ Option.when(
        sim.banks.exists(bank =>
          bank.nplAmount != PLN.Zero ||
            bank.consumerNpl != PLN.Zero ||
            bank.loansShort != PLN.Zero ||
            bank.loansMedium != PLN.Zero ||
            bank.loansLong != PLN.Zero,
        ),
      )(UnsupportedFamilyId.BankCreditRiskState)
      ++ Option.when(sim.world.gov.cumulativeDebt != PLN.Zero)(UnsupportedFamilyId.GovernmentFiscalCumulativeDebt)
      ++ Option.when(sim.world.nbp.qeCumulative != PLN.Zero)(UnsupportedFamilyId.NbpQeCumulativePurchases)
      ++ Option.when(sim.world.social.jst.debt != PLN.Zero)(UnsupportedFamilyId.JstDebt)
      ++ Option.when(
        sim.world.financial.quasiFiscal.bankHoldings != PLN.Zero ||
          sim.world.financial.quasiFiscal.nbpHoldings != PLN.Zero,
      )(UnsupportedFamilyId.QuasiFiscalHolderSplits)

  enum RuntimeShellCategory:
    case ExecutionShell
    case SettlementShell

  case class RuntimeShell(
      sector: EntitySector,
      index: Int,
      name: String,
      category: RuntimeShellCategory,
  )

  // Dynamic-sector shell indices come from a zero-population runtime
  // topology. Real runtime positions are derived per step by
  // RuntimeLedgerTopology.fromState(...).
  val nonPersistedRuntimeShells: Vector[RuntimeShell] = Vector(
    RuntimeShell(
      EntitySector.Households,
      zeroPopulationTopology.households.aggregate,
      "Households.Aggregate",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Households,
      zeroPopulationTopology.households.landlords,
      "Households.Landlords",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Households,
      zeroPopulationTopology.households.depositors,
      "Households.Depositors",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Households,
      zeroPopulationTopology.households.investors,
      "Households.Investors",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      zeroPopulationTopology.firms.aggregate,
      "Firms.Aggregate",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      zeroPopulationTopology.firms.services,
      "Firms.Services",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      zeroPopulationTopology.firms.capitalGoods,
      "Firms.CapitalGoods",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      zeroPopulationTopology.firms.ioCounterparty,
      "Firms.IoCounterparty",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      zeroPopulationTopology.firms.domesticDemand,
      "Firms.DomesticDemand",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Banks,
      zeroPopulationTopology.banks.aggregate,
      "Banks.Aggregate",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      TreasuryRuntimeContract.TreasuryBudgetSettlement.sector,
      TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
      TreasuryRuntimeContract.TreasuryBudgetSettlement.name,
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      TreasuryRuntimeContract.TaxpayerCollection.sector,
      TreasuryRuntimeContract.TaxpayerCollection.index,
      TreasuryRuntimeContract.TaxpayerCollection.name,
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      NbpRuntimeContract.ReserveSettlementLiability.sector,
      NbpRuntimeContract.ReserveSettlementLiability.index,
      NbpRuntimeContract.ReserveSettlementLiability.name,
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      NbpRuntimeContract.StandingFacilityBackstop.sector,
      NbpRuntimeContract.StandingFacilityBackstop.index,
      NbpRuntimeContract.StandingFacilityBackstop.name,
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      EntitySector.Insurance,
      zeroPopulationTopology.insurance.aggregate,
      "Insurance.Aggregate",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Funds,
      zeroPopulationTopology.funds.bondholders,
      "Funds.Bondholders",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Funds,
      zeroPopulationTopology.funds.bondMarket,
      "Funds.BondMarket",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Funds,
      zeroPopulationTopology.funds.markets,
      "Funds.Markets",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Funds,
      zeroPopulationTopology.funds.healthcare,
      "Funds.Healthcare",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Foreign,
      ForeignRuntimeContract.TradeSettlement.index,
      ForeignRuntimeContract.TradeSettlement.name,
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      EntitySector.Foreign,
      ForeignRuntimeContract.IncomeSettlement.index,
      ForeignRuntimeContract.IncomeSettlement.name,
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      EntitySector.Foreign,
      ForeignRuntimeContract.CapitalSettlement.index,
      ForeignRuntimeContract.CapitalSettlement.name,
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      EntitySector.Foreign,
      ForeignRuntimeContract.TransferSettlement.index,
      ForeignRuntimeContract.TransferSettlement.name,
      RuntimeShellCategory.SettlementShell,
    ),
  )

  def publicAsset(asset: AssetType): PublicAssetContract =
    publicAssetsByAsset(asset)

  def isSupportedPersistedPair(
      sector: EntitySector,
      asset: AssetType,
      index: Int,
  ): Boolean =
    supportedPairs.iterator
      .filter(_.asset == asset)
      .exists(_.matches(sector, index))

  def requireSupportedPersistedPair(
      sector: EntitySector,
      asset: AssetType,
      index: Int,
      context: String,
  ): Unit =
    require(
      isSupportedPersistedPair(sector, asset, index),
      s"$context attempted to use unsupported ledger pair ($sector, $asset, $index).",
    )

end AssetOwnershipContract
