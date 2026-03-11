package sfc.engine

import sfc.accounting.*
import sfc.agents.*
import sfc.config.SimParams
import sfc.engine.markets.{CorporateBondMarket, EquityMarket, GvcTrade, HousingMarket}
import sfc.engine.mechanisms.{Expectations, Macroprudential, SectoralMobility}
import sfc.types.*

/** Immutable snapshot of the entire simulation state at the end of one month.
  *
  * Fields with defaults (`hhAgg`, `households`, `monetaryAgg`, `bop`) are
  * populated during the step pipeline and do not need to be provided at init.
  */
case class World(
    month: Int,                                         // simulation month (1-indexed)
    inflation: Rate,                                    // CPI YoY inflation
    priceLevel: Double,                                 // cumulative CPI index (base = 1.0)
    gdpProxy: Double,                                   // monthly GDP proxy
    currentSigmas: Vector[Double],                      // per-sector σ (Arthur increasing returns)
    totalPopulation: Int,                               // employed + immigrants + retirees
    gov: GovState,                                      // government budget & debt
    nbp: Nbp.State,                                     // central bank: rate, bonds, FX, QE
    bank: BankingAggregate,                             // consolidated banking balance sheet
    bankingSector: Banking.State,                       // multi-bank: individual states, interbank, term structure
    forex: ForexState,                                  // EUR/PLN, exports, imports, trade balance
    bop: BopState = BopState.zero,                      // balance of payments: NFA, CA, KA, FDI
    hhAgg: Household.Aggregates,                        // household aggregates (employment, wages, consumption)
    households: Vector[Household.State] = Vector.empty, // individual household states
    monetaryAgg: Option[MonetaryAggregates] = None,     // M1, monetary base, credit multiplier (CREDIT_DIAGNOSTICS)
    social: SocialState,                                // JST, ZUS, PPK, demographics
    financial: FinancialMarketsState,                   // equity, corporate bonds, insurance, TFI
    external: ExternalState,                            // GVC, immigration, tourism
    real: RealState,                                    // housing, mobility, investment, energy, automation
    mechanisms: MechanismsState,                        // macropru, expectations, BFG, informal economy
    plumbing: MonetaryPlumbingState,                    // reserve corridor, standing facilities, interbank
    flows: FlowState,                                   // single-step flows → SFC identities
):
  def updateSocial(f: SocialState => SocialState): World                        = copy(social = f(social))
  def updateFinancial(f: FinancialMarketsState => FinancialMarketsState): World = copy(financial = f(financial))
  def updateExternal(f: ExternalState => ExternalState): World                  = copy(external = f(external))
  def updateReal(f: RealState => RealState): World                              = copy(real = f(real))
  def updateMechanisms(f: MechanismsState => MechanismsState): World            = copy(mechanisms = f(mechanisms))
  def updatePlumbing(f: MonetaryPlumbingState => MonetaryPlumbingState): World  = copy(plumbing = f(plumbing))
  def updateFlows(f: FlowState => FlowState): World                             = copy(flows = f(flows))

// ---------------------------------------------------------------------------
// Nested state types
// ---------------------------------------------------------------------------

/** Social security system and local government state. */
case class SocialState(
    jst: Jst.State,                                // local government (JST): budget, debt, deposits
    zus: SocialSecurity.ZusState,                  // ZUS: contributions, pensions, FUS balance
    ppk: SocialSecurity.PpkState,                  // PPK: employee contributions, gov bond portfolio
    demographics: SocialSecurity.DemographicsState, // working-age, retirees, monthly retirements
)
object SocialState:
  val zero: SocialState = SocialState(
    jst = Jst.State.zero,
    zus = SocialSecurity.ZusState.zero,
    ppk = SocialSecurity.PpkState.zero,
    demographics = SocialSecurity.DemographicsState.zero,
  )

/** Non-bank financial sector state. */
case class FinancialMarketsState(
    equity: EquityMarket.State,                // GPW: index, market cap, returns, dividends
    corporateBonds: CorporateBondMarket.State, // Catalyst: outstanding, YTM, spread, holdings
    insurance: Insurance.State,                // life/non-life reserves, three-asset allocation
    nbfi: Nbfi.State,                          // TFI: AUM, NBFI credit, deposit drain
)
object FinancialMarketsState:
  val zero: FinancialMarketsState = FinancialMarketsState(
    equity = EquityMarket.zero,
    corporateBonds = CorporateBondMarket.zero,
    insurance = Insurance.State.zero,
    nbfi = Nbfi.State.zero,
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
    automationRatio: Ratio = Ratio.Zero,      // share of Automated firms
    hybridRatio: Ratio = Ratio.Zero,          // share of Hybrid firms
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
    effectiveShadowShare: Double = 0.0, // consumption-weighted shadow share
)
object MechanismsState:
  val zero: MechanismsState = MechanismsState(
    macropru = Macroprudential.State.zero,
    expectations = Expectations.zero,
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

/** Single-step flow outputs — recomputed each step, zero at init. Feed into SFC
  * identities.
  */
case class FlowState(
    ioFlows: PLN = PLN.Zero,                                                                // I-O intermediate payments between sectors
    fdiProfitShifting: PLN = PLN.Zero,                                                      // intangible imports booked abroad (profit shifting)
    fdiRepatriation: PLN = PLN.Zero,                                                        // dividend repatriation by foreign-owned firms
    fdiCitLoss: PLN = PLN.Zero,                                                             // CIT lost to profit shifting
    diasporaRemittanceInflow: PLN = PLN.Zero,                                               // diaspora remittance inflow
    tourismExport: PLN = PLN.Zero,                                                          // inbound tourism services export
    tourismImport: PLN = PLN.Zero,                                                          // outbound tourism services import
    aggInventoryStock: PLN = PLN.Zero,                                                      // aggregate firm inventory stock
    aggInventoryChange: PLN = PLN.Zero,                                                     // ΔInventories (enters GDP)
    aggEnergyCost: PLN = PLN.Zero,                                                          // aggregate energy + CO₂ costs
    firmBirths: Int = 0,                                                                    // new firms (recycled bankrupt slots)
    firmDeaths: Int = 0,                                                                    // firms bankrupt this step
    taxEvasionLoss: PLN = PLN.Zero,                                                         // tax lost to 4-channel evasion (CIT+VAT+PIT+excise)
    informalEmployed: Double = 0.0,                                                         // estimated informal employment count
    bailInLoss: PLN = PLN.Zero,                                                             // bail-in capital loss on bank creditors
    bfgLevyTotal: Double = 0.0,                                                             // BFG resolution levy from all banks
    sectorDemandMult: Vector[Double] = Vector.fill(SimParams.DefaultSectorDefs.length)(1.0), // per-sector demand multipliers from S4
)
object FlowState:
  val zero: FlowState = FlowState()
