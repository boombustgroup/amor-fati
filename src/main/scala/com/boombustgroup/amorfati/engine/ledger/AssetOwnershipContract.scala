package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.{AggregateBatchContract, FlowSimulation}
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

  case class SupportedPair(
      sector: EntitySector,
      asset: AssetType,
  )

  enum PublicAssetStatus:
    case SupportedPersistedStock
    case PublicAssetWithoutEngineContract

  case class PublicAssetContract(
      asset: AssetType,
      status: PublicAssetStatus,
      supportedSectors: Set[EntitySector],
      note: String,
  )

  val publicAssets: Vector[PublicAssetContract] = Vector(
    PublicAssetContract(
      AssetType.DemandDeposit,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Households, EntitySector.Banks),
      "Household savings and bank demand-deposit liabilities.",
    ),
    PublicAssetContract(
      AssetType.TermDeposit,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Banks),
      "Bank term-deposit liabilities.",
    ),
    PublicAssetContract(
      AssetType.FirmLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Firms, EntitySector.Banks),
      "Firm debt mirrored by bank loan assets.",
    ),
    PublicAssetContract(
      AssetType.ConsumerLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Households, EntitySector.Banks),
      "Household consumer debt mirrored by bank loan assets.",
    ),
    PublicAssetContract(
      AssetType.MortgageLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Households),
      "Currently only the household-side mortgage liability is in the supported slice.",
    ),
    PublicAssetContract(
      AssetType.GovBondAFS,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Banks),
      "Bank-only AFS government-bond holdings.",
    ),
    PublicAssetContract(
      AssetType.GovBondHTM,
      PublicAssetStatus.SupportedPersistedStock,
      Set(
        EntitySector.Banks,
        EntitySector.Government,
        EntitySector.Foreign,
        EntitySector.NBP,
        EntitySector.Insurance,
        EntitySector.Funds,
      ),
      "Issuer outstanding plus holder stocks across supported sectors.",
    ),
    PublicAssetContract(
      AssetType.CorpBond,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Firms, EntitySector.Banks, EntitySector.Insurance, EntitySector.Funds),
      "Firm bond liabilities plus supported holder stocks.",
    ),
    PublicAssetContract(
      AssetType.Reserve,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Banks),
      "Bank reserve asset; the NBP-side settlement liability is not a supported stock yet.",
    ),
    PublicAssetContract(
      AssetType.StandingFacility,
      PublicAssetStatus.PublicAssetWithoutEngineContract,
      Set.empty,
      "The engine uses standing-facility flow mechanisms but does not persist this as a stock pair.",
    ),
    PublicAssetContract(
      AssetType.InterbankLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Banks),
      "Aggregate net interbank position per bank.",
    ),
    PublicAssetContract(
      AssetType.Equity,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Households, EntitySector.Firms, EntitySector.Insurance, EntitySector.Funds),
      "Supported stock family despite separate timing-cleanup work for equity flows.",
    ),
    PublicAssetContract(
      AssetType.LifeReserve,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Insurance),
      "Insurance-side life reserve stock.",
    ),
    PublicAssetContract(
      AssetType.NonLifeReserve,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Insurance),
      "Insurance-side non-life reserve stock.",
    ),
    PublicAssetContract(
      AssetType.TfiUnit,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Funds),
      "TFI AUM proxy inside the NBFI fund bucket.",
    ),
    PublicAssetContract(
      AssetType.NbfiLoan,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Funds),
      "Used for both NBFI credit and quasi-fiscal loan portfolio inside the Funds sector.",
    ),
    PublicAssetContract(
      AssetType.Cash,
      PublicAssetStatus.SupportedPersistedStock,
      Set(EntitySector.Firms, EntitySector.Funds),
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
      Set(EntitySector.NBP),
      "NBP FX reserve stock.",
    ),
  )

  private val publicAssetsByAsset = publicAssets.map(contract => contract.asset -> contract).toMap

  require(
    publicAssets.map(_.asset).toSet == AssetType.values.toSet,
    "AssetOwnershipContract must classify every public AssetType.",
  )

  val supportedPairs: Set[SupportedPair] =
    publicAssets.iterator
      .filter(_.status == PublicAssetStatus.SupportedPersistedStock)
      .flatMap: contract =>
        contract.supportedSectors.iterator.map(sector => SupportedPair(sector, contract.asset))
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

  val nonPersistedRuntimeShells: Vector[RuntimeShell] = Vector(
    RuntimeShell(
      EntitySector.Households,
      AggregateBatchContract.HouseholdIndex.Aggregate,
      "Households.Aggregate",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Households,
      AggregateBatchContract.HouseholdIndex.Landlords,
      "Households.Landlords",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Households,
      AggregateBatchContract.HouseholdIndex.Depositors,
      "Households.Depositors",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Households,
      AggregateBatchContract.HouseholdIndex.Investors,
      "Households.Investors",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      AggregateBatchContract.FirmIndex.Aggregate,
      "Firms.Aggregate",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      AggregateBatchContract.FirmIndex.Services,
      "Firms.Services",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      AggregateBatchContract.FirmIndex.CapitalGoods,
      "Firms.CapitalGoods",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      AggregateBatchContract.FirmIndex.IoCounterparty,
      "Firms.IoCounterparty",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Firms,
      AggregateBatchContract.FirmIndex.DomesticDemand,
      "Firms.DomesticDemand",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Banks,
      AggregateBatchContract.BankIndex.Aggregate,
      "Banks.Aggregate",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Government,
      AggregateBatchContract.GovernmentIndex.Budget,
      "Government.Budget",
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      EntitySector.Government,
      AggregateBatchContract.GovernmentIndex.TaxpayerPool,
      "Government.TaxpayerPool",
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      EntitySector.NBP,
      AggregateBatchContract.NbpIndex.Aggregate,
      "NBP.Aggregate",
      RuntimeShellCategory.SettlementShell,
    ),
    RuntimeShell(
      EntitySector.Insurance,
      AggregateBatchContract.InsuranceIndex.Aggregate,
      "Insurance.Aggregate",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Funds,
      AggregateBatchContract.FundIndex.Bondholders,
      "Funds.Bondholders",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Funds,
      AggregateBatchContract.FundIndex.BondMarket,
      "Funds.BondMarket",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Funds,
      AggregateBatchContract.FundIndex.Markets,
      "Funds.Markets",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Funds,
      AggregateBatchContract.FundIndex.Healthcare,
      "Funds.Healthcare",
      RuntimeShellCategory.ExecutionShell,
    ),
    RuntimeShell(
      EntitySector.Foreign,
      AggregateBatchContract.ForeignIndex.Aggregate,
      "Foreign.Aggregate",
      RuntimeShellCategory.SettlementShell,
    ),
  )

  def publicAsset(asset: AssetType): PublicAssetContract =
    publicAssetsByAsset(asset)

  def isSupportedPersistedPair(
      sector: EntitySector,
      asset: AssetType,
  ): Boolean =
    supportedPairs.contains(SupportedPair(sector, asset))

  def requireSupportedPersistedPair(
      sector: EntitySector,
      asset: AssetType,
      context: String,
  ): Unit =
    require(
      isSupportedPersistedPair(sector, asset),
      s"$context attempted to use unsupported ledger pair ($sector, $asset).",
    )

end AssetOwnershipContract
