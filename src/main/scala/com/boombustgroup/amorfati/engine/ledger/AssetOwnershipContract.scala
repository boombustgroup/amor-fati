package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.{FlowSimulation, RuntimeLedgerTopology}
import com.boombustgroup.amorfati.types.{PLN, Share}
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

  /** Logical owner slot in the runtime ledger topology.
    *
    * Dynamic slots match every runtime entity in a sector, for example all
    * households or all banks. Fixed slots identify one stable aggregate owner,
    * such as the single NBP account or a named Funds bucket.
    */
  enum SectorId:
    /** Matches any runtime owner index inside `sector`. */
    case Dynamic(sector: EntitySector)

    /** Matches exactly one owner index inside `sector`. */
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

  /** Expanded `(owner, asset)` contract row used by runtime membership checks.
    *
    * These pairs are derived from [[PublicAssetContract.supportedSlots]] for
    * assets whose status is [[PublicAssetStatus.SupportedPersistedStock]].
    */
  case class SupportedPair(
      owner: SectorId,
      asset: AssetType,
  ):
    def matches(
        sector: EntitySector,
        index: Int,
    ): Boolean =
      owner.matches(sector, index)

  /** Current engine contract status for a public `amor-fati-ledger` asset type.
    *
    * The public ledger API is wider than the engine's supported persisted
    * ownership slice. This status records whether an asset is in the supported
    * slice, deliberately outside it for now, or only exposed by the public API.
    */
  enum PublicAssetStatus:
    /** Asset has an engine-owned persisted stock contract and participates in
      * the current supported ledger slice.
      */
    case SupportedPersistedStock

    /** Asset maps to real persisted state in the engine, but that stock family
      * is intentionally outside the supported ledger-owned slice for now.
      */
    case UnsupportedPersistedStock

    /** Asset is part of the public ledger API, but the engine currently has no
      * persisted ownership contract for it.
      */
    case PublicAssetWithoutEngineContract

  /** One registry row for a public ledger asset type.
    *
    * @param asset
    *   public asset identifier from the ledger module
    * @param status
    *   how the engine currently treats this asset family
    * @param supportedSlots
    *   owner slots associated with the asset; for supported persisted stocks
    *   these are the contracted owners, while for unsupported persisted stocks
    *   they document where the legacy state still lives
    * @param note
    *   short human-facing explanation for audits and tests
    */
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

  /** Complete asset registry. Every public `AssetType` must appear exactly
    * once, enforced by the `require`s below and by
    * `AssetOwnershipContractSpec`.
    */
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
        fund(FundRuntimeIndex.Ppk),
        fund(FundRuntimeIndex.Nbfi),
        fund(FundRuntimeIndex.QuasiFiscal),
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
        fund(FundRuntimeIndex.Ppk),
        fund(FundRuntimeIndex.CorpBondOther),
        fund(FundRuntimeIndex.Nbfi),
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
        fund(FundRuntimeIndex.Nbfi),
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
      Set(fund(FundRuntimeIndex.Nbfi)),
      "TFI AUM proxy inside the NBFI fund bucket.",
    ),
    PublicAssetContract(
      AssetType.NbfiLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(
        fund(FundRuntimeIndex.Nbfi),
        fund(FundRuntimeIndex.QuasiFiscal),
      ),
      "Used for both NBFI credit and quasi-fiscal loan portfolio inside the Funds sector.",
    ),
    PublicAssetContract(
      AssetType.Cash,
      PublicAssetStatus.SupportedPersistedStock,
      Set(
        dynamic(EntitySector.Firms),
        fund(FundRuntimeIndex.Zus),
        fund(FundRuntimeIndex.Nfz),
        fund(FundRuntimeIndex.Fp),
        fund(FundRuntimeIndex.Pfron),
        fund(FundRuntimeIndex.Fgsp),
        fund(FundRuntimeIndex.Jst),
        fund(FundRuntimeIndex.Nbfi),
      ),
      "Supported only for firm cash and selected fund buckets; not a universal settlement cash asset.",
    ),
    PublicAssetContract(
      AssetType.Capital,
      PublicAssetStatus.UnsupportedPersistedStock,
      Set(dynamic(EntitySector.Banks)),
      "Used for persisted bank capital, but not part of the supported ledger-owned stock slice.",
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

  /** Flattened supported owner/asset pairs consumed by runtime contract checks.
    *
    * Unsupported and orphan public assets are intentionally absent from this
    * set even when `publicAssets` records their current engine location.
    */
  val supportedPairs: Set[SupportedPair] =
    publicAssetsByAsset.valuesIterator
      .filter(_.status == PublicAssetStatus.SupportedPersistedStock)
      .flatMap: contract =>
        contract.supportedSlots.iterator.map(slot => SupportedPair(slot, contract.asset))
      .toSet

  /** Public asset types with no persisted ownership contract in the engine. */
  val orphanPublicAssets: Set[AssetType] =
    publicAssets.iterator
      .filter(_.status == PublicAssetStatus.PublicAssetWithoutEngineContract)
      .map(_.asset)
      .toSet

  /** Reason a persisted or stock-like family is outside the supported slice. */
  enum UnsupportedCategory:
    /** Real persisted engine state exists, but is not a supported ledger-owned
      * transferable asset family yet.
      */
    case UnsupportedPersistedStock

    /** Aggregate accounting, policy, or market metric that may look stock-like
      * but is not a holder-resolved asset balance.
      */
    case MetricOnly

  /** Stable identifiers for non-supported stock families tracked by the audit.
    *
    * Add a value here when a stock-like field remains outside `supportedPairs`;
    * this keeps known gaps explicit instead of letting them hide in `World` or
    * agent state.
    */
  enum UnsupportedFamilyId:
    case BankCapital
    case BankCreditRiskState
    case EquityForeignOwnershipShare
    case BopExternalPositionMetrics
    case GovernmentFiscalCumulativeDebt
    case NbpQeCumulativePurchases
    case JstDebt

  /** Audit registry entry for a stock-like family outside `supportedPairs`.
    *
    * @param id
    *   stable identifier used by tests and runtime scans
    * @param category
    *   whether this is unsupported persisted state or metric-only memory
    * @param note
    *   concise explanation of why the family is outside the supported slice
    */
  case class UnsupportedFamily(
      id: UnsupportedFamilyId,
      category: UnsupportedCategory,
      note: String,
  )

  /** Explicit list of known non-supported stock-like families.
    *
    * This is an audit allow-list, not an ownership source. Supported financial
    * owners must be represented through `publicAssets`/`supportedPairs`.
    */
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
      UnsupportedFamilyId.EquityForeignOwnershipShare,
      UnsupportedCategory.MetricOnly,
      "GPW foreign-ownership share is market memory for dividend/BoP splitting, not a holder-resolved equity stock.",
    ),
    UnsupportedFamily(
      UnsupportedFamilyId.BopExternalPositionMetrics,
      UnsupportedCategory.MetricOnly,
      "BoP foreign-asset, liability, reserve, and NFA fields are aggregate external-position metrics; ledger-owned NBP FX reserves live in LedgerFinancialState.",
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
  )

  /** Detect which unsupported families are present in a concrete runtime state.
    *
    * The scan is intentionally shallow and conservative: it flags known legacy
    * or metric fields when they carry non-zero values so tests can ensure the
    * unsupported-family registry stays aligned with observable state.
    */
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
      ++ Option.when(sim.world.financialMarkets.equity.foreignOwnership != Share.Zero)(UnsupportedFamilyId.EquityForeignOwnershipShare)
      ++ Option.when(
        sim.world.bop.nfa != PLN.Zero ||
          sim.world.bop.foreignAssets != PLN.Zero ||
          sim.world.bop.foreignLiabilities != PLN.Zero ||
          sim.world.bop.reserves != PLN.Zero,
      )(UnsupportedFamilyId.BopExternalPositionMetrics)
      ++ Option.when(sim.world.gov.cumulativeDebt != PLN.Zero)(UnsupportedFamilyId.GovernmentFiscalCumulativeDebt)
      ++ Option.when(sim.world.nbp.qeCumulative != PLN.Zero)(UnsupportedFamilyId.NbpQeCumulativePurchases)
      ++ Option.when(sim.world.social.jst.debt != PLN.Zero)(UnsupportedFamilyId.JstDebt)

  /** Kind of non-persisted runtime account used during flow execution. */
  enum RuntimeShellCategory:
    /** Aggregate or synthetic node used to calculate and route flows inside one
      * step; it is not persisted as an owner after the step.
      */
    case ExecutionShell

    /** Settlement counterparty for double-entry flow mechanics; it records the
      * other side of a transaction without becoming a persisted stock owner.
      */
    case SettlementShell

  /** Non-persisted runtime ledger node.
    *
    * Runtime shells may appear in emitted flows, but they are excluded from
    * `supportedPairs` because they do not own end-of-month financial stocks.
    *
    * @param sector
    *   ledger sector used by emitted flows
    * @param index
    *   runtime index within the sector
    * @param name
    *   stable diagnostic name used in tests and audit output
    * @param category
    *   execution or settlement role
    */
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

  /** Lookup helper for the single registry row associated with `asset`. */
  def publicAsset(asset: AssetType): PublicAssetContract =
    publicAssetsByAsset(asset)

  /** True when the concrete runtime owner slot is part of the supported
    * persisted ownership contract for `asset`.
    */
  def isSupportedPersistedPair(
      sector: EntitySector,
      asset: AssetType,
      index: Int,
  ): Boolean =
    supportedPairs.iterator
      .filter(_.asset == asset)
      .exists(_.matches(sector, index))

end AssetOwnershipContract
