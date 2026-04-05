package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, EquityMarket, FiscalBudget, GvcTrade, HousingMarket, OpenEconomy}
import com.boombustgroup.amorfati.engine.mechanisms.{Expectations, Macroprudential, SectoralMobility}
import com.boombustgroup.amorfati.types.*

/** Immutable snapshot of the entire simulation state at the end of one month.
  *
  * Fields with defaults (`bop`) are populated during the step pipeline and do
  * not need to be provided at init.
  */
case class World(
    month: Int,                                                        // simulation month (1-indexed)
    inflation: Rate,                                                   // CPI YoY inflation
    priceLevel: Double,                                                // cumulative CPI index (base = 1.0)
    currentSigmas: Vector[Sigma],                                      // per-sector σ (Arthur increasing returns)
    gov: FiscalBudget.GovState,                                        // government budget & debt
    nbp: Nbp.State,                                                    // central bank: rate, bonds, FX, QE
    bankingSector: Banking.MarketState,                                // banking macro state: interbank conditions, configs, term structure
    forex: OpenEconomy.ForexState,                                     // EUR/PLN, exports, imports, trade balance
    bop: OpenEconomy.BopState = OpenEconomy.BopState.zero,             // balance of payments: NFA, CA, KA, FDI
    householdMarket: HouseholdMarketState = HouseholdMarketState.zero, // explicit household wage-market state used in hot paths
    social: SocialState,                                               // JST, ZUS, PPK, demographics
    financial: FinancialMarketsState,                                  // equity, corporate bonds, insurance, TFI
    external: ExternalState,                                           // GVC, immigration, tourism
    real: RealState,                                                   // housing, mobility, investment, energy, automation
    mechanisms: MechanismsState,                                       // macropru, expectations, BFG, informal economy
    plumbing: MonetaryPlumbingState,                                   // reserve corridor, standing facilities, interbank
    pipeline: PipelineState = PipelineState.zero,                      // inter-step demand / hiring / fiscal signals
    flows: FlowState,                                                  // single-step derived flow outputs → SFC identities
    regionalWages: Map[Region, PLN] = Map.empty,                       // per-region wage levels (NUTS-1)
):
  def derivedTotalPopulation: Int =
    social.demographics.workingAgePop + social.demographics.retirees

  def cachedMonthlyGdpProxy: PLN = flows.monthlyGdpProxy

  def updateSocial(f: SocialState => SocialState): World                        = copy(social = f(social))
  def updateFinancial(f: FinancialMarketsState => FinancialMarketsState): World = copy(financial = f(financial))
  def updateExternal(f: ExternalState => ExternalState): World                  = copy(external = f(external))
  def updateReal(f: RealState => RealState): World                              = copy(real = f(real))
  def updateMechanisms(f: MechanismsState => MechanismsState): World            = copy(mechanisms = f(mechanisms))
  def updatePlumbing(f: MonetaryPlumbingState => MonetaryPlumbingState): World  = copy(plumbing = f(plumbing))
  def updatePipeline(f: PipelineState => PipelineState): World                  = copy(pipeline = f(pipeline))
  def updateFlows(f: FlowState => FlowState): World                             = copy(flows = f(flows))

// ---------------------------------------------------------------------------
// Nested state types
// ---------------------------------------------------------------------------

/** Social security system and local government state. */
case class SocialState(
    jst: Jst.State,                                 // local government (JST): budget, debt, deposits
    zus: SocialSecurity.ZusState,                   // ZUS: contributions, pensions, FUS balance
    nfz: SocialSecurity.NfzState,                   // NFZ: health insurance contributions, spending, balance
    ppk: SocialSecurity.PpkState,                   // PPK: employee contributions, gov bond portfolio
    demographics: SocialSecurity.DemographicsState, // working-age, retirees, monthly retirements
    earmarked: EarmarkedFunds.State,                // FP, PFRON, FGŚP
)
object SocialState:
  val zero: SocialState = SocialState(
    jst = Jst.State.zero,
    zus = SocialSecurity.ZusState.zero,
    nfz = SocialSecurity.NfzState.zero,
    ppk = SocialSecurity.PpkState.zero,
    demographics = SocialSecurity.DemographicsState.zero,
    earmarked = EarmarkedFunds.State.zero,
  )

object World:
  def apply(
      month: Int,
      inflation: Rate,
      priceLevel: Double,
      gdpProxy: Double,
      currentSigmas: Vector[Sigma],
      totalPopulation: Int,
      gov: FiscalBudget.GovState,
      nbp: Nbp.State,
      bankingSector: Banking.MarketState,
      forex: OpenEconomy.ForexState,
      hhAgg: Household.Aggregates,
      social: SocialState,
      financial: FinancialMarketsState,
      external: ExternalState,
      real: RealState,
      mechanisms: MechanismsState,
      plumbing: MonetaryPlumbingState,
      flows: FlowState,
  ): World =
    val compatDemographics =
      if social.demographics == SocialSecurity.DemographicsState.zero && totalPopulation > 0
      then social.demographics.copy(workingAgePop = totalPopulation)
      else social.demographics
    val compatFlows        =
      if flows.monthlyGdpProxy == PLN.Zero && gdpProxy > 0.0 then flows.copy(monthlyGdpProxy = PLN(gdpProxy))
      else flows
    new World(
      month = month,
      inflation = inflation,
      priceLevel = priceLevel,
      currentSigmas = currentSigmas,
      gov = gov,
      nbp = nbp,
      bankingSector = bankingSector,
      forex = forex,
      bop = OpenEconomy.BopState.zero,
      householdMarket = HouseholdMarketState.fromAggregates(hhAgg),
      social = social.copy(demographics = compatDemographics),
      financial = financial,
      external = external,
      real = real,
      mechanisms = mechanisms,
      plumbing = plumbing,
      pipeline = PipelineState.zero,
      flows = compatFlows,
      regionalWages = Map.empty,
    )

/** Explicit household labor-market state carried outside aggregate caches. */
case class HouseholdMarketState(
    marketWage: PLN,
    reservationWage: PLN,
)
object HouseholdMarketState:
  val zero: HouseholdMarketState = HouseholdMarketState(PLN.Zero, PLN.Zero)

  def fromAggregates(agg: Household.Aggregates): HouseholdMarketState =
    HouseholdMarketState(
      marketWage = agg.marketWage,
      reservationWage = agg.reservationWage,
    )

/** Non-bank financial sector state. */
case class FinancialMarketsState(
    equity: EquityMarket.State,                // GPW: index, market cap, returns, dividends
    corporateBonds: CorporateBondMarket.State, // Catalyst: outstanding, YTM, spread, holdings
    insurance: Insurance.State,                // life/non-life reserves, three-asset allocation
    nbfi: Nbfi.State,                          // TFI: AUM, NBFI credit, deposit drain
    quasiFiscal: QuasiFiscal.State,            // BGK/PFR: off-balance-sheet bonds, subsidized lending
)
object FinancialMarketsState:
  val zero: FinancialMarketsState = FinancialMarketsState(
    equity = EquityMarket.zero,
    corporateBonds = CorporateBondMarket.zero,
    insurance = Insurance.State.zero,
    nbfi = Nbfi.State.zero,
    quasiFiscal = QuasiFiscal.State.zero,
  )

/** Structural external-sector state carried across steps. */
case class ExternalState(
    gvc: GvcTrade.State,                // GVC: disruption, foreign prices, sector trade
    immigration: Immigration.State,     // immigrant stock, monthly flows, remittances
    tourismSeasonalFactor: Double = 1.0, // seasonal multiplier (base = 1.0)
)
object ExternalState:
  val zero: ExternalState = ExternalState(
    gvc = GvcTrade.zero,
    immigration = Immigration.State.zero,
  )

/** Real economy state — physical and wealth structure. */
case class RealState(
    housing: HousingMarket.State,             // price index, mortgage stock, regional sub-markets
    sectoralMobility: SectoralMobility.State, // cross-sector hires, quits, mobility rate
    grossInvestment: PLN = PLN.Zero,          // aggregate GFCF by firms
    aggGreenInvestment: PLN = PLN.Zero,       // green investment (renewables, energy efficiency)
    aggGreenCapital: PLN = PLN.Zero,          // green capital stock across all firms
    etsPrice: Double = 0.0,                   // EU ETS allowance price (EUR/tCO₂)
    automationRatio: Share = Share.Zero,      // share of Automated firms
    hybridRatio: Share = Share.Zero,          // share of Hybrid firms
)
object RealState:
  val zero: RealState = RealState(
    housing = HousingMarket.zero,
    sectoralMobility = SectoralMobility.zero,
  )

/** Macro-mechanism state — policies and endogenous phenomena carried across
  * steps.
  */
case class MechanismsState(
    macropru: Macroprudential.State,   // CCyB, credit-to-GDP gap
    expectations: Expectations.State,  // inflation forecast, credibility, forward guidance
    bfgFundBalance: PLN = PLN.Zero,    // cumulative BFG resolution fund
    informalCyclicalAdj: Double = 0.0, // smoothed cyclical shadow-economy adjustment
    nextTaxShadowShare: Double = 0.0,  // next-period smoothed tax-side shadow share
)
object MechanismsState:
  def zero(using SimParams): MechanismsState = MechanismsState(
    macropru = Macroprudential.State.zero,
    expectations = Expectations.initial,
  )

/** NBP monetary plumbing — single-step flows from S8/S9, surfaced for
  * SimOutput.
  */
case class MonetaryPlumbingState(
    reserveInterestTotal: PLN = PLN.Zero, // NBP interest on required reserves
    standingFacilityNet: PLN = PLN.Zero,  // net standing facility income (deposit − Lombard)
    interbankInterestNet: PLN = PLN.Zero, // net interbank interest flows
    depositFacilityUsage: PLN = PLN.Zero, // voluntary reserves at NBP above minimum
    fofResidual: PLN = PLN.Zero,          // flow-of-funds residual
)
object MonetaryPlumbingState:
  val zero: MonetaryPlumbingState = MonetaryPlumbingState()

/** Inter-step pipeline signals carried into the next month. */
case class PipelineState(
    sectorDemandMult: Vector[Double] = Vector.fill(SimParams.DefaultSectorDefs.length)(1.0),     // per-sector demand multipliers from S4
    sectorDemandPressure: Vector[Double] = Vector.fill(SimParams.DefaultSectorDefs.length)(1.0), // uncapped demand/capacity ratios for hiring
    sectorHiringSignal: Vector[Double] = Vector.fill(SimParams.DefaultSectorDefs.length)(1.0),   // smoothed sector hiring signal used by firm labor planning
    fiscalRuleSeverity: Int = 0,                                                                 // 0=none, 1=SRW, 2=SGP, 3=Art86_55, 4=Art216_60
    govSpendingCutRatio: Share = Share.Zero,                                                     // fraction of raw spending cut by fiscal rules
    aggregateHiringSlack: Double = 1.0,                                                          // economy-wide compression of firm labor targets when plans exceed supply
    startupAbsorptionRate: Double = 1.0,                                                         // share of startup hiring targets filled across active startup firms
)
object PipelineState:
  val zero: PipelineState = PipelineState()

/** Single-step derived flow outputs — recomputed each step, zero at init. Feed
  * into SFC identities and output columns.
  */
case class FlowState(
    monthlyGdpProxy: PLN = PLN.Zero,          // cached monthly GDP proxy for diagnostics / output ratios
    ioFlows: PLN = PLN.Zero,                  // I-O intermediate payments between sectors
    fdiProfitShifting: PLN = PLN.Zero,        // intangible imports booked abroad (profit shifting)
    fdiRepatriation: PLN = PLN.Zero,          // dividend repatriation by foreign-owned firms
    fdiCitLoss: PLN = PLN.Zero,               // CIT lost to profit shifting
    diasporaRemittanceInflow: PLN = PLN.Zero, // diaspora remittance inflow
    tourismExport: PLN = PLN.Zero,            // inbound tourism services export
    tourismImport: PLN = PLN.Zero,            // outbound tourism services import
    aggInventoryStock: PLN = PLN.Zero,        // aggregate firm inventory stock
    aggInventoryChange: PLN = PLN.Zero,       // ΔInventories (enters GDP)
    aggEnergyCost: PLN = PLN.Zero,            // aggregate energy + CO₂ costs
    firmBirths: Int = 0,                      // new firms (recycled + net new)
    firmDeaths: Int = 0,                      // firms bankrupt this step
    netFirmBirths: Int = 0,                   // net new firms appended to vector
    taxEvasionLoss: PLN = PLN.Zero,           // tax lost to 4-channel evasion (CIT+VAT+PIT+excise)
    realizedTaxShadowShare: Double = 0.0,     // current-period realized aggregate tax-side shadow share
    bailInLoss: PLN = PLN.Zero,               // bail-in capital loss on bank creditors
    bfgLevyTotal: Double = 0.0,               // BFG resolution levy from all banks
)
object FlowState:
  val zero: FlowState = FlowState()
