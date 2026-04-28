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
    inflation: Rate,                                                                  // CPI YoY inflation
    priceLevel: PriceIndex,                                                           // cumulative CPI index (base = 1.0)
    currentSigmas: Vector[Sigma],                                                     // per-sector σ (Arthur increasing returns)
    gov: FiscalBudget.GovState,                                                       // government budget & debt
    nbp: Nbp.State,                                                                   // central bank: rate, QE regime, monthly FX operations
    bankingSector: Banking.MarketState,                                               // banking macro state: interbank conditions, configs, term structure
    forex: OpenEconomy.ForexState,                                                    // EUR/PLN, exports, imports, trade balance
    bop: OpenEconomy.BopState = OpenEconomy.BopState.zero,                            // balance of payments: NFA, CA, KA, FDI
    householdMarket: HouseholdMarketState = HouseholdMarketState.zero,                // explicit household wage-market state used in hot paths
    social: SocialState,                                                              // JST, ZUS, PPK, demographics
    financialMarkets: FinancialMarketsState,                                          // financial-market memory; ownership lives in LedgerFinancialState
    external: ExternalState,                                                          // GVC, immigration, tourism
    real: RealState,                                                                  // housing, mobility, investment, energy, automation
    mechanisms: MechanismsState,                                                      // macropru, expectations, BFG, informal economy
    plumbing: MonetaryPlumbingState,                                                  // reserve corridor, standing facilities, interbank
    pipeline: PipelineState = PipelineState.zero(SimParams.DefaultSectorDefs.length), // inter-step demand / hiring / fiscal signals
    flows: FlowState,                                                                 // single-step derived flow outputs → SFC identities
    regionalWages: Map[Region, PLN] = Map.empty,                                      // per-region wage levels (NUTS-1)
):
  def seedIn: DecisionSignals = pipeline.seedIn

  def derivedTotalPopulation: Int =
    social.demographics.workingAgePop + social.demographics.retirees

  def laborForcePopulation: Int =
    social.demographics.workingAgePop.max(1)

  def unemploymentRate(employed: Int): Share =
    Share.One - Share.fraction(employed, laborForcePopulation)

  def cachedMonthlyGdpProxy: PLN = flows.monthlyGdpProxy

// ---------------------------------------------------------------------------
// Nested state types
// ---------------------------------------------------------------------------

/** Social security system and local government state. */
case class SocialState(
    jst: Jst.State,                                 // local government (JST): budget and debt; cash lives in LedgerFinancialState
    zus: SocialSecurity.ZusState,                   // ZUS: contributions, pensions, FUS balance
    nfz: SocialSecurity.NfzState,                   // NFZ: health insurance contributions, spending, balance
    ppk: SocialSecurity.PpkState,                   // PPK monthly contribution flow; holdings live in LedgerFinancialState
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

/** Financial-market memory carried by `World`.
  *
  * This is deliberately not the ownership ledger. Keep market prices,
  * last-month diagnostics, and unsupported transition fields here; keep
  * ledger-contracted financial stocks in `LedgerFinancialState`.
  */
case class FinancialMarketsState(
    equity: EquityMarket.State,                // GPW market prices, returns, issuance, dividends
    corporateBonds: CorporateBondMarket.State, // Catalyst pricing and last-month diagnostics
    insurance: Insurance.State,                // monthly premium, claims, investment-income diagnostics
    nbfi: Nbfi.State,                          // monthly TFI/NBFI origination, defaults, deposit-drain diagnostics
    quasiFiscal: QuasiFiscal.State,            // quasi-fiscal monthly issuance/lending diagnostics
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
    gvc: GvcTrade.State,                               // GVC: disruption, foreign prices, sector trade
    immigration: Immigration.State,                    // immigrant stock, monthly flows, remittances
    tourismSeasonalFactor: Multiplier = Multiplier.One, // seasonal multiplier (base = 1.0)
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
    etsPrice: Multiplier = Multiplier.Zero,   // EU ETS allowance price (EUR/tCO2)
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
    macropru: Macroprudential.State,         // CCyB, credit-to-GDP gap
    expectations: Expectations.State,        // inflation forecast, credibility, forward guidance
    bfgFundBalance: PLN = PLN.Zero,          // cumulative BFG resolution fund
    informalCyclicalAdj: Share = Share.Zero, // smoothed cyclical shadow-economy adjustment
    nextTaxShadowShare: Share = Share.Zero,  // next-period smoothed tax-side shadow share
)
object MechanismsState:
  def zero(using SimParams): MechanismsState = MechanismsState(
    macropru = Macroprudential.State.zero,
    expectations = Expectations.initial,
  )

/** NBP monetary plumbing */
case class MonetaryPlumbingState(
    reserveInterestTotal: PLN = PLN.Zero, // NBP interest on required reserves
    standingFacilityNet: PLN = PLN.Zero,  // net standing facility income (deposit − Lombard)
    interbankInterestNet: PLN = PLN.Zero, // net interbank interest flows
    depositFacilityUsage: PLN = PLN.Zero, // voluntary reserves at NBP above minimum
    fofResidual: PLN = PLN.Zero,          // flow-of-funds residual
)
object MonetaryPlumbingState:
  val zero: MonetaryPlumbingState = MonetaryPlumbingState()

/** Explicit informational surface available at the start of a month.
  *
  * This is the persisted `pre` side of the timing contract. Same-month
  * operational artifacts belong on [[OperationalSignals]], not here.
  */
case class DecisionSignals(
    unemploymentRate: Share,
    inflation: Rate,
    expectedInflation: Rate,
    laggedHiringSlack: Share,
    startupAbsorptionRate: Share,
    sectorDemandMult: Vector[Multiplier],
    sectorDemandPressure: Vector[Multiplier],
    sectorHiringSignal: Vector[Multiplier],
)
object DecisionSignals:
  def zero(sectorCount: Int): DecisionSignals =
    DecisionSignals(
      unemploymentRate = Share.Zero,
      inflation = Rate.Zero,
      expectedInflation = Rate.Zero,
      laggedHiringSlack = Share.One,
      startupAbsorptionRate = Share.One,
      sectorDemandMult = Vector.fill(sectorCount)(Multiplier.One),
      sectorDemandPressure = Vector.fill(sectorCount)(Multiplier.One),
      sectorHiringSignal = Vector.fill(sectorCount)(Multiplier.One),
    )

/** Inter-step pipeline signals carried into the next month.
  *
  * This remains the persisted substrate, while [[World.seedIn]] exposes the
  * narrower decision-oriented surface consumed by timing-sensitive blocks.
  * Same-month operational consumers should prefer [[OperationalSignals]]
  * instead of reading directly from this structure.
  */
case class PipelineState(
    sectorDemandMult: Vector[Multiplier],       // per-sector demand multipliers from S4
    sectorDemandPressure: Vector[Multiplier],   // uncapped demand/capacity ratios for hiring
    sectorHiringSignal: Vector[Multiplier],     // smoothed sector hiring signal used by firm labor planning
    fiscalRuleSeverity: Int = 0,                // 0=none, 1=SRW, 2=SGP, 3=Art86_55, 4=Art216_60
    govSpendingCutRatio: Share = Share.Zero,    // fraction of raw spending cut by fiscal rules
    laggedHiringSlack: Share = Share.One,       // month t labor-tightness signal carried into month t+1 macro decisions
    operationalHiringSlack: Share = Share.One,  // persisted snapshot of month-t labor compression for observability / compatibility
    startupAbsorptionRate: Share = Share.One,   // share of startup hiring targets filled across active startup firms
    laggedUnemploymentRate: Share = Share.Zero, // end-of-month unemployment extracted for next-month decisions
    laggedInflation: Rate = Rate.Zero,          // realized inflation lagged into next month
    laggedExpectedInflation: Rate = Rate.Zero,  // expected inflation lagged into next month
):
  def seedIn: DecisionSignals =
    DecisionSignals(
      unemploymentRate = laggedUnemploymentRate,
      inflation = laggedInflation,
      expectedInflation = laggedExpectedInflation,
      laggedHiringSlack = laggedHiringSlack,
      startupAbsorptionRate = startupAbsorptionRate,
      sectorDemandMult = sectorDemandMult,
      sectorDemandPressure = sectorDemandPressure,
      sectorHiringSignal = sectorHiringSignal,
    )

  def withDecisionSignals(signals: DecisionSignals): PipelineState =
    copy(
      sectorDemandMult = signals.sectorDemandMult,
      sectorDemandPressure = signals.sectorDemandPressure,
      sectorHiringSignal = signals.sectorHiringSignal,
      laggedHiringSlack = signals.laggedHiringSlack,
      startupAbsorptionRate = signals.startupAbsorptionRate,
      laggedUnemploymentRate = signals.unemploymentRate,
      laggedInflation = signals.inflation,
      laggedExpectedInflation = signals.expectedInflation,
    )

object PipelineState:
  def zero(sectorCount: Int): PipelineState =
    PipelineState(
      sectorDemandMult = Vector.fill(sectorCount)(Multiplier.One),
      sectorDemandPressure = Vector.fill(sectorCount)(Multiplier.One),
      sectorHiringSignal = Vector.fill(sectorCount)(Multiplier.One),
    )

  def bootstrap(
      sectorCount: Int,
      unemploymentRate: Share,
      inflation: Rate,
      expectedInflation: Rate,
  ): PipelineState =
    zero(sectorCount).copy(
      laggedUnemploymentRate = unemploymentRate,
      laggedInflation = inflation,
      laggedExpectedInflation = expectedInflation,
    )

  def zero(using p: SimParams): PipelineState = zero(p.sectorDefs.length)

/** Single-step derived flow outputs — recomputed each step, zero at init. Feed
  * into SFC identities and output columns.
  */
case class FlowState(
    monthlyGdpProxy: PLN = PLN.Zero,            // cached monthly GDP proxy for diagnostics / output ratios
    sectorOutputs: Vector[PLN] = Vector.empty,  // nominal monthly output by schema sector
    ioFlows: PLN = PLN.Zero,                    // I-O intermediate payments between sectors
    fdiProfitShifting: PLN = PLN.Zero,          // intangible imports booked abroad (profit shifting)
    fdiRepatriation: PLN = PLN.Zero,            // dividend repatriation by foreign-owned firms
    fdiCitLoss: PLN = PLN.Zero,                 // CIT lost to profit shifting
    diasporaRemittanceInflow: PLN = PLN.Zero,   // diaspora remittance inflow
    tourismExport: PLN = PLN.Zero,              // inbound tourism services export
    tourismImport: PLN = PLN.Zero,              // outbound tourism services import
    aggInventoryStock: PLN = PLN.Zero,          // aggregate firm inventory stock
    aggInventoryChange: PLN = PLN.Zero,         // ΔInventories (enters GDP)
    aggEnergyCost: PLN = PLN.Zero,              // aggregate energy + CO₂ costs
    firmBirths: Int = 0,                        // new firms (recycled + net new)
    firmDeaths: Int = 0,                        // firms bankrupt this step
    netFirmBirths: Int = 0,                     // net new firms appended to vector
    taxEvasionLoss: PLN = PLN.Zero,             // tax lost to 4-channel evasion (CIT+VAT+PIT+excise)
    realizedTaxShadowShare: Share = Share.Zero, // current-period realized aggregate tax-side shadow share
    bailInLoss: PLN = PLN.Zero,                 // bail-in capital loss on bank creditors
    bfgLevyTotal: PLN = PLN.Zero,               // BFG resolution levy from all banks
)
object FlowState:
  val zero: FlowState = FlowState()
