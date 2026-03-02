package sfc.engine

import sfc.agents.{HhState, HhAggregates, Household, NbpState, JstState, ZusState, PpkState, DemographicsState, ImmigrationState}
import sfc.sfc.{GovState, BankState, ForexState, BopState, MonetaryAggregates}
import sfc.engine.{BankingSectorState, EquityMarketState, HousingMarketState, SectoralMobilityState}

case class World(
  month: Int,
  inflation: Double,
  priceLevel: Double,
  gov: GovState,
  nbp: NbpState,
  bank: BankState,
  forex: ForexState,
  hh: HhState,
  automationRatio: Double,
  hybridRatio: Double,
  gdpProxy: Double,
  currentSigmas: Vector[Double],
  ioFlows: Double = 0.0,
  bop: BopState = BopState.zero,
  hhAgg: Option[HhAggregates] = None,
  households: Option[Vector[Household]] = None,
  bankingSector: Option[BankingSectorState] = None,
  monetaryAgg: Option[MonetaryAggregates] = None,
  jst: JstState = JstState.zero,
  zus: ZusState = ZusState.zero,
  ppk: PpkState = PpkState.zero,
  demographics: DemographicsState = DemographicsState.zero,
  macropru: MacropruState = MacropruState.zero,
  equity: EquityMarketState = EquityMarket.zero,
  housing: HousingMarketState = HousingMarket.zero,
  sectoralMobility: SectoralMobilityState = SectoralMobility.zero,
  gvc: GvcState = ExternalSector.zero,
  expectations: ExpectationsState = Expectations.zero,
  immigration: ImmigrationState = ImmigrationState.zero,
  corporateBonds: CorporateBondMarketState = CorporateBondMarket.zero,
  insurance: InsuranceSectorState = InsuranceSector.zero,
  sectorDemandMult: Vector[Double] = Vector.fill(6)(1.0),
  fofResidual: Double = 0.0,
  grossInvestment: Double = 0.0
)
