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
    /** Current simulation month number (1-indexed after the first step). */
    month: Int,
    /** Current CPI inflation rate (year-on-year, updated each month). */
    inflation: Rate,
    /** Cumulative price level index (base = 1.0). */
    priceLevel: Double,
    /** Monthly GDP proxy (sum of firm revenues × demand multipliers). */
    gdpProxy: Double,
    /** Per-sector technological sophistication σ (Arthur-style increasing
      * returns).
      */
    currentSigmas: Vector[Double],
    /** Total population (employed + immigrants + retirees). */
    totalPopulation: Int,
    /** Government sector: tax revenue, cumulative debt, bonds outstanding. */
    gov: GovState,
    /** NBP central bank: reference rate, bond holdings, FX reserves, QE state.
      */
    nbp: Nbp.State,
    /** Banking sector aggregate balance sheet (consolidated across all
      * commercial banks).
      */
    bank: BankingAggregate,
    /** Multi-bank state: individual bank states, interbank rate, term
      * structure.
      */
    bankingSector: Banking.State,
    /** Foreign exchange market: EUR/PLN rate, exports, imports, trade balance.
      */
    forex: ForexState,
    /** Balance of payments: NFA, current account, capital account, FDI. */
    bop: BopState = BopState.zero,
    /** Household aggregates (employment, wages, consumption, distributional).
      */
    hhAgg: Household.Aggregates,
    /** Vector of individual household states. Empty at initialisation. */
    households: Vector[Household.State] = Vector.empty,
    /** Monetary aggregates (M1, monetary base, credit multiplier). None when
      * CREDIT_DIAGNOSTICS=false.
      */
    monetaryAgg: Option[MonetaryAggregates] = None,
    /** Social security system: local government, ZUS, PPK, demographic
      * structure.
      */
    social: SocialState,
    /** Non-bank financial sector: equity market, corporate bonds, insurance,
      * TFI.
      */
    financial: FinancialMarketsState,
    /** Structural external-sector state carried across steps: GVC, immigration,
      * tourism.
      */
    external: ExternalState,
    /** Real economy: housing, labour mobility, gross investment, energy,
      * automation.
      */
    real: RealState,
    /** Macro mechanisms: macroprudential policy, expectations, BFG fund,
      * informal economy (lagged state).
      */
    mechanisms: MechanismsState,
    /** NBP monetary plumbing: reserve corridor, standing facilities, interbank
      * flows.
      */
    plumbing: MonetaryPlumbingState,
    /** Single-step flow outputs — recomputed in S10; feed into SFC identities.
      */
    flows: FlowState,
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
    /** Local government (JST): sub-national budget, debt, bank deposits. */
    jst: Jst.State,
    /** ZUS: current flows (contributions, pension payments, government subsidy)
      * and FUS balance.
      */
    zus: SocialSecurity.ZusState,
    /** PPK: monthly employee contributions and government bond portfolio. */
    ppk: SocialSecurity.PpkState,
    /** Demographic structure: working-age population, retirees, monthly
      * retirements.
      */
    demographics: SocialSecurity.DemographicsState,
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
    /** WSE equity market: index, market cap, returns, dividends, household
      * equity wealth.
      */
    equity: EquityMarket.State,
    /** Corporate bond market: outstanding stock, YTM, credit spread, bank and
      * PPK holdings.
      */
    corporateBonds: CorporateBondMarket.State,
    /** Insurance sector: life/non-life reserves, three-asset allocation,
      * premium and claims flows.
      */
    insurance: Insurance.State,
    /** Shadow banking / TFI: AUM, NBFI credit stock, deposit drain from
      * commercial banks.
      */
    nbfi: Nbfi.State,
)
object FinancialMarketsState:
  val zero: FinancialMarketsState = FinancialMarketsState(
    equity = EquityMarket.zero,
    corporateBonds = CorporateBondMarket.zero,
    insurance = Insurance.State.zero,
    nbfi = Nbfi.State.zero,
  )

/** Structural external-sector state carried across steps (not recomputed from
  * scratch).
  */
case class ExternalState(
    /** Global value chains: disruption index, foreign price index, sector
      * exports/imports.
      */
    gvc: GvcTrade.State,
    /** Immigration: immigrant stock, monthly flows, outward remittances. */
    immigration: Immigration.State,
    /** Tourism seasonal multiplier carried from the previous step (base = 1.0).
      */
    tourismSeasonalFactor: Double = 1.0,
)
object ExternalState:
  val zero: ExternalState = ExternalState(
    gvc = GvcTrade.zero,
    immigration = Immigration.State.zero,
  )

/** Real economy state — physical and wealth structure. */
case class RealState(
    /** Housing market: price index, mortgage stock, regional sub-markets. */
    housing: HousingMarket.State,
    /** Sectoral labour mobility: cross-sector hires, voluntary quits, mobility
      * rate.
      */
    sectoralMobility: SectoralMobility.State,
    /** Aggregate gross fixed capital formation (GFCF) by firms in the current
      * step.
      */
    grossInvestment: PLN = PLN.Zero,
    /** Aggregate green investment by firms (renewables, energy efficiency) in
      * the current step.
      */
    aggGreenInvestment: PLN = PLN.Zero,
    /** Aggregate green capital stock across all living firms. */
    aggGreenCapital: PLN = PLN.Zero,
    /** Current EU ETS allowance price (EUR/tCO₂, drifts 3%/year). Zero when
      * ENERGY=false.
      */
    etsPrice: Double = 0.0,
    /** Share of fully automated firms (TechState.Automated) among living firms.
      */
    automationRatio: Ratio = Ratio.Zero,
    /** Share of hybrid firms (TechState.Hybrid) among living firms. */
    hybridRatio: Ratio = Ratio.Zero,
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
    /** Macroprudential policy: counter-cyclical capital buffer (CCyB),
      * credit-to-GDP gap.
      */
    macropru: Macroprudential.State,
    /** Inflation expectations: forecast, credibility index, forward guidance
      * rate, forecast error.
      */
    expectations: Expectations.State,
    /** Cumulative BFG bank resolution fund balance (accumulates from monthly
      * bank levies).
      */
    bfgFundBalance: PLN = PLN.Zero,
    /** Smoothed cyclical shadow-economy adjustment (lagged EWM; higher when
      * unemployment rises).
      */
    informalCyclicalAdj: Double = 0.0,
    /** Consumption-weighted effective shadow-economy share across sectors. */
    effectiveShadowShare: Double = 0.0,
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
    /** Total interest on required reserves paid by NBP to commercial banks. */
    reserveInterestTotal: PLN = PLN.Zero,
    /** Net bank income from NBP standing facilities (overnight deposit minus
      * Lombard credit).
      */
    standingFacilityNet: PLN = PLN.Zero,
    /** Net interest flows in the interbank market (creditors minus debtors). */
    interbankInterestNet: PLN = PLN.Zero,
    /** Total voluntary reserves held by banks at NBP above the required
      * minimum.
      */
    depositFacilityUsage: PLN = PLN.Zero,
    /** Flow-of-funds residual — mismatch between sectoral demand and productive
      * capacity.
      */
    fofResidual: PLN = PLN.Zero,
)
object MonetaryPlumbingState:
  val zero: MonetaryPlumbingState = MonetaryPlumbingState()

/** Single-step flow outputs — recomputed in S10, zero at init. "Flow" means
  * recomputed each step; several fields feed directly into SFC identities.
  */
case class FlowState(
    /** Total intermediate payments between sectors (I-O matrix × capacity ×
      * price level).
      */
    ioFlows: PLN = PLN.Zero,
    /** Intangible service imports booked abroad by foreign-owned firms (profit
      * shifting).
      */
    fdiProfitShifting: PLN = PLN.Zero,
    /** Dividend repatriation by foreign-owned firms to parent companies. */
    fdiRepatriation: PLN = PLN.Zero,
    /** CIT revenue lost due to profit shifting (fdiProfitShifting × CIT rate).
      */
    fdiCitLoss: PLN = PLN.Zero,
    /** Inflow of remittances from the Polish diaspora abroad. Zero when
      * REMITTANCE=false.
      */
    diasporaRemittanceInflow: PLN = PLN.Zero,
    /** Tourism services export (inbound visitors to Poland). Zero when
      * TOURISM=false.
      */
    tourismExport: PLN = PLN.Zero,
    /** Tourism services import (Polish residents travelling abroad). Zero when
      * TOURISM=false.
      */
    tourismImport: PLN = PLN.Zero,
    /** Aggregate inventory stock across all living firms. Zero when
      * INVENTORY=false.
      */
    aggInventoryStock: PLN = PLN.Zero,
    /** Change in aggregate inventories this step (enters GDP via expenditure
      * approach).
      */
    aggInventoryChange: PLN = PLN.Zero,
    /** Aggregate energy and CO₂ costs across all firms. Zero when ENERGY=false.
      */
    aggEnergyCost: PLN = PLN.Zero,
    /** Number of new firms entering the market (recycled bankrupt slots). Zero
      * when FIRM_ENTRY=false.
      */
    firmBirths: Int = 0,
    /** Number of firms that went bankrupt this step. */
    firmDeaths: Int = 0,
    /** Total tax revenue lost across four evasion channels (CIT + VAT + PIT +
      * excise).
      */
    taxEvasionLoss: PLN = PLN.Zero,
    /** Estimated informal employment count (formally employed ×
      * effectiveShadowShare). Not a monetary amount.
      */
    informalEmployed: Double = 0.0,
    /** Capital loss imposed on bank creditors via bail-in (bondholders and
      * uninsured depositors).
      */
    bailInLoss: PLN = PLN.Zero,
    /** Total BFG resolution levy collected from all operating banks this step.
      */
    bfgLevyTotal: Double = 0.0,
    /** Per-sector demand multipliers from S4 — threaded into Firm.process via
      * macro4firms.copy.
      */
    sectorDemandMult: Vector[Double] = Vector.fill(SimParams.DefaultSectorDefs.length)(1.0),
)
object FlowState:
  val zero: FlowState = FlowState()
