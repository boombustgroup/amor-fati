package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.{OperationalSignals, World}
import com.boombustgroup.amorfati.engine.markets.{CalvoPricing, CorporateBondMarket, IntermediateMarket, LaborMarket}
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

/** Firm sector economics — production, I-O intermediate market, CAPEX
  * decisions, financing splits (equity/bonds/bank loans), labor matching, NPL
  * detection.
  *
  * Own Input contract — does not depend on Step.Output types in its public API.
  * Step Output types are constructed internally from raw values.
  */
object FirmEconomics:

  // ---- Calibration constants ----
  private val BondRevertThreshold = 0.001 // minimum revert ratio to trigger bond-to-loan reversion

  // ---- Accumulated flows (monoid on PLN) ----

  /** Accumulated monetary flows from firm processing — one per firm, reduced
    * via `+`.
    */
  private case class FirmFlows(
      tax: PLN,             // CIT paid (after informal evasion)
      capex: PLN,           // technology upgrade CAPEX (AI or hybrid)
      techImp: PLN,         // import content of CAPEX (forex demand)
      newLoans: PLN,        // new bank loans net of equity/bond splits
      equityIssuance: PLN,  // GPW equity raised this month
      grossInvestment: PLN, // physical capital investment
      bondIssuance: PLN,    // corporate bond issuance (pre-absorption)
      profitShifting: PLN,  // FDI profit shifting outflow
      fdiRepatriation: PLN, // FDI dividend repatriation outflow
      inventoryChange: PLN, // net inventory change (+ accumulation, - drawdown)
      citEvasion: PLN,      // CIT evaded via informal economy
      energyCost: PLN,      // total energy + ETS cost
      greenInvestment: PLN, // green capital investment
      principalRepaid: PLN, // firm loan principal repaid
  ):
    def +(o: FirmFlows): FirmFlows = FirmFlows(
      tax + o.tax,
      capex + o.capex,
      techImp + o.techImp,
      newLoans + o.newLoans,
      equityIssuance + o.equityIssuance,
      grossInvestment + o.grossInvestment,
      bondIssuance + o.bondIssuance,
      profitShifting + o.profitShifting,
      fdiRepatriation + o.fdiRepatriation,
      inventoryChange + o.inventoryChange,
      citEvasion + o.citEvasion,
      energyCost + o.energyCost,
      greenInvestment + o.greenInvestment,
      principalRepaid + o.principalRepaid,
    )

  private object FirmFlows:
    val zero: FirmFlows = FirmFlows(
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
    )

  // ---- Internal phase result types ----

  /** Per-bank lending rates and credit approval, shared across phases. */
  private case class LendingConditions(
      firmWorld: World,                       // world with current-month wages for firm decision-making
      executionMonth: ExecutionMonth,         // realized month currently being executed
      operationalSignals: OperationalSignals, // same-month demand / labor surface for incumbent firms
      rates: Vector[Rate],                    // per-bank lending rates
      bankCanLend: (Int, PLN) => Boolean,     // credit approval: (bankId, amount) => approved?
      nBanks: Int,                            // number of banks in multi-bank system
  )

  /** Per-firm processing outcome — immutable, one per firm. */
  private case class FirmOutcome(
      firm: Firm.State,           // updated firm state after decision + financing
      flows: FirmFlows,           // monetary flows from this firm
      realizedPostTaxProfit: PLN, // realized monthly profit after tax, floored at zero
      bankId: BankId,             // relationship bank (for per-bank aggregation)
      finalLoan: PLN,             // bank loan after equity/bond splits
      bondAmt: PLN,               // corporate bond issuance (pre-absorption)
      principalRepaid: PLN,       // scheduled loan principal repaid this month
  )

  /** Result of per-firm processing (phase 2). */
  private case class FirmProcessingResult(
      outcomes: Vector[FirmOutcome],    // per-firm immutable outcomes
      flows: FirmFlows,                 // aggregate flows (monoid sum)
      firmBondAmounts: Map[FirmId, PLN], // per-firm bond issuance for reversion
  )

  /** Result of bond absorption (phase 3). */
  private case class BondAbsorptionResult(
      firms: Vector[Firm.State], // firms after bond reversion (unsold → bank loans)
      sumNewLoans: PLN,          // total new bank loans incl. reversion
      corpBondAbsorption: Share, // Catalyst absorption ratio (0-1)
      actualBondIssuance: PLN,   // bonds issued after absorption constraint
  )

  /** Financing split: how a firm's CAPEX loan is divided across three channels.
    */
  private case class FinancingSplit(
      bankLoan: PLN,   // remainder after equity and bond channels
      equity: PLN,     // GPW equity issuance (large firms only)
      bonds: PLN,      // Catalyst corporate bonds (medium+ firms only)
      firm: Firm.State, // firm with updated debt/equity/bondDebt
  )

  /** Result of NPL and interest computation (phase 6). */
  private case class NplResult(
      nplNew: PLN,                   // new NPL volume from bankruptcies
      nplLoss: PLN,                  // NPL loss net of recovery
      totalBondDefault: PLN,         // bond default from bankrupt firms
      firmDeaths: Int,               // count of newly bankrupt firms
      intIncome: PLN,                // aggregate bank interest income
      perBankNplDebt: Vector[PLN],   // NPL debt by bank index
      perBankIntIncome: Vector[PLN], // interest income by bank index
      perBankWorkers: Vector[Int],   // worker count by bank index
  )

  // ---- Public I/O types ----

  /** Raw inputs needed from previous calculus stages. */
  case class Input(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      // From FiscalConstraint
      month: ExecutionMonth,
      lendingBaseRate: Rate,
      resWage: PLN,
      baseMinWage: PLN,
      minWagePriceLevel: PriceIndex,
      // From Labor
      newWage: PLN,
      employed: Int,
      laborDemand: Int,
      wageGrowth: Coefficient,
      immigration: Immigration.State,
      netMigration: Int,
      demographics: SocialSecurity.DemographicsState,
      zusState: SocialSecurity.ZusState,
      nfzState: SocialSecurity.NfzState,
      ppkState: SocialSecurity.PpkState,
      rawPpkBondPurchase: PLN,
      earmarked: EarmarkedFunds.State,
      living: Vector[Firm.State],
      regionalWages: Map[Region, PLN],
      // From HouseholdIncome
      hhOutput: HouseholdIncomeEconomics.Output,
      operationalSignals: OperationalSignals,
      // From Demand
      avgDemandMult: Multiplier,
      sectorCapReal: Vector[PLN],
      govPurchases: PLN,
      laggedInvestDemand: PLN,
      fiscalRuleStatus: com.boombustgroup.amorfati.engine.markets.FiscalRules.RuleStatus,
      rng: RandomStream,
  )

  /** Full step output — all fields previously in FirmProcessingStep.Output. */
  case class StepOutput(
      ioFirms: Vector[Firm.State],         // firms after I-O intermediate market
      households: Vector[Household.State], // households after labor matching + immigration
      sumTax: PLN,                         // aggregate CIT paid
      sumCapex: PLN,                       // aggregate technology CAPEX
      sumTechImp: PLN,                     // aggregate technology imports
      sumNewLoans: PLN,                    // aggregate new bank loans (incl. bond reversion)
      sumEquityIssuance: PLN,              // aggregate GPW equity raised
      sumGrossInvestment: PLN,             // aggregate physical capital investment
      sumBondIssuance: PLN,                // aggregate bond issuance (pre-absorption)
      sumProfitShifting: PLN,              // aggregate FDI profit shifting
      sumFdiRepatriation: PLN,             // aggregate FDI repatriation
      sumInventoryChange: PLN,             // aggregate net inventory change
      sumCitEvasion: PLN,                  // aggregate CIT evasion
      sumEnergyCost: PLN,                  // aggregate energy + ETS cost
      sumGreenInvestment: PLN,             // aggregate green capital investment
      totalIoPaid: PLN,                    // total intermediate goods payments
      nplNew: PLN,                         // new non-performing loan volume
      nplLoss: PLN,                        // NPL loss net of recovery
      totalBondDefault: PLN,               // bond default from bankrupt firms
      firmDeaths: Int,                     // number of firms that went bankrupt
      intIncome: PLN,                      // aggregate bank interest income
      corpBondAbsorption: Share,           // Catalyst absorption ratio (0-1)
      actualBondIssuance: PLN,             // bond issuance after absorption constraint
      netMigration: Int,                   // net immigration (inflow - outflow)
      perBankNewLoans: Vector[PLN],        // new loans by bank index
      sumFirmPrincipal: PLN,               // aggregate firm loan principal repaid
      perBankFirmPrincipal: Vector[PLN],   // firm principal repaid by bank index
      perBankNplDebt: Vector[PLN],         // NPL debt by bank index
      perBankIntIncome: Vector[PLN],       // interest income by bank index
      perBankWorkers: Vector[Int],         // worker count by bank index
      lendingRates: Vector[Rate],          // per-bank lending rates
      postFirmCrossSectorHires: Int,       // cross-sector hires in labor matching
      markupInflation: Rate,               // Calvo: annualized revenue-weighted avg markup change
      sumRealizedPostTaxProfit: PLN,       // aggregate realized post-tax profits from Firm.process
      sumStateOwnedPostTaxProfit: PLN,     // aggregate realized post-tax profits of SOEs
  )

  case class Result(
      tax: PLN,
      capex: PLN,
      newLoans: PLN,
      equityIssuance: PLN,
      grossInvestment: PLN,
      bondIssuance: PLN,
      actualBondIssuance: PLN,
      profitShifting: PLN,
      fdiRepatriation: PLN,
      ioPayments: PLN,
      nplLoss: PLN,
      intIncome: PLN,
      firmPrincipal: PLN,
      greenInvestment: PLN,
      energyCost: PLN,
      firmDeaths: Int,
      nplNew: PLN,
      totalBondDefault: PLN,
      corpBondAbsorption: Share,
      // Non-monetary outputs needed by later stages
      markupInflation: Rate,
      ioFirms: Vector[Firm.State],
      updatedHouseholds: Vector[Household.State],
      crossSectorHires: Int,
      techImports: PLN,
      inventoryChange: PLN,
      citEvasion: PLN,
      perBankWorkers: Vector[Int],
      realizedPostTaxProfit: PLN,
  )

  def toResult(s5: StepOutput): Result = Result(
    tax = s5.sumTax,
    capex = s5.sumCapex,
    newLoans = s5.sumNewLoans,
    equityIssuance = s5.sumEquityIssuance,
    grossInvestment = s5.sumGrossInvestment,
    bondIssuance = s5.sumBondIssuance,
    actualBondIssuance = s5.actualBondIssuance,
    profitShifting = s5.sumProfitShifting,
    fdiRepatriation = s5.sumFdiRepatriation,
    ioPayments = s5.totalIoPaid,
    nplLoss = s5.nplLoss,
    intIncome = s5.intIncome,
    firmPrincipal = s5.sumFirmPrincipal,
    greenInvestment = s5.sumGreenInvestment,
    energyCost = s5.sumEnergyCost,
    firmDeaths = s5.firmDeaths,
    nplNew = s5.nplNew,
    totalBondDefault = s5.totalBondDefault,
    corpBondAbsorption = s5.corpBondAbsorption,
    markupInflation = s5.markupInflation,
    ioFirms = s5.ioFirms,
    updatedHouseholds = s5.households,
    crossSectorHires = s5.postFirmCrossSectorHires,
    techImports = s5.sumTechImp,
    inventoryChange = s5.sumInventoryChange,
    citEvasion = s5.sumCitEvasion,
    perBankWorkers = s5.perBankWorkers,
    realizedPostTaxProfit = s5.sumRealizedPostTaxProfit,
  )

  // ---- Entry point (from step outputs, used by FlowSimulation) ----

  /** Run the full firm processing pipeline from step-level inputs. This is the
    * direct replacement for FirmProcessingStep.run().
    */
  def runStep(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      s1: FiscalConstraintEconomics.Output,
      s2: LaborEconomics.Output,
      s3: HouseholdIncomeEconomics.Output,
      s4: DemandEconomics.Output,
      rng: RandomStream,
  )(using p: SimParams): StepOutput =
    val stepIn = StepInput(w, firms, households, banks, s1, s2, s3, s4)
    runInternal(stepIn, rng)

  // ---- Entry point (from raw values, used by MonthlyCalculus) ----

  def compute(in: Input)(using p: SimParams): StepOutput =
    // Construct bridge types from raw values
    val s1 = FiscalConstraintEconomics.Output(in.month, in.baseMinWage, in.minWagePriceLevel, in.resWage, in.lendingBaseRate)
    val s2 = LaborEconomics.Output(
      in.newWage,
      in.employed,
      in.laborDemand,
      in.wageGrowth,
      in.operationalSignals.operationalHiringSlack,
      in.immigration,
      in.netMigration,
      in.demographics,
      in.zusState,
      in.nfzState,
      in.ppkState,
      in.rawPpkBondPurchase,
      in.earmarked,
      in.living,
      in.regionalWages,
    )
    val s4 = DemandEconomics.Output(
      in.govPurchases,
      in.operationalSignals.sectorDemandMult,
      in.operationalSignals.sectorDemandPressure,
      in.operationalSignals.sectorHiringSignal,
      in.avgDemandMult,
      in.sectorCapReal,
      in.laggedInvestDemand,
      in.fiscalRuleStatus,
    )

    val stepIn = StepInput(in.w, in.firms, in.households, in.banks, s1, s2, in.hhOutput, s4)
    runInternal(stepIn, in.rng)

  // ---- Core pipeline (shared by compute + runStep) ----

  private def runInternal(stepIn: StepInput, rng: RandomStream)(using p: SimParams): StepOutput =
    val lending                             = prepareLending(stepIn, rng)
    val fp                                  = processFirms(stepIn.firms, lending, rng)
    val bonded                              = applyBondAbsorption(fp, stepIn.w, stepIn.banks)
    val (ioFirms, totalIoPaid)              = applyIntermediateMarket(bonded.firms, stepIn)
    // Calvo staggered pricing: per-firm markup update
    val calvoFirms                          = ioFirms.map: f =>
      val sectorMult         = stepIn.s4.sectorMults(f.sector.toInt)
      val passthrough        =
        if f.stateOwned then StateOwned.effectiveEnergyPassthrough(f.sector.toInt)
        else Share.One
      val energyCostPressure =
        CalvoPricing.energyCostPressure(
          stepIn.w.external.gvc.commodityPriceIndex,
          p.climate.energyCostShares(f.sector.toInt),
          passthrough,
        )
      val calvo              = CalvoPricing.updateFirmMarkup(f.markup, sectorMult, stepIn.s2.wageGrowth, energyCostPressure, rng)
      f.copy(markup = calvo.newMarkup)
    val (finalHouseholds, crossSectorHires) = processLaborMarket(calvoFirms, stepIn, rng)
    val npl                                 = computeNplAndInterest(stepIn.firms, calvoFirms, lending)
    val markupInfl                          = CalvoPricing.aggregateMarkupInflation(calvoFirms, ioFirms).annualize
    assembleOutput(fp, bonded, calvoFirms, totalIoPaid, finalHouseholds, crossSectorHires, npl, stepIn, lending, markupInfl)

  // ---- Internal step input (mirrors old FirmProcessingStep.Input) ----

  private case class StepInput(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      s1: FiscalConstraintEconomics.Output,
      s2: LaborEconomics.Output,
      s3: HouseholdIncomeEconomics.Output,
      s4: DemandEconomics.Output,
  )

  // ---- Phase 1: Lending conditions ----

  /** Prepare per-bank rates, lending functions, and world snapshot with updated
    * wages for firm decision-making.
    */
  private def prepareLending(in: StepInput, rng: RandomStream)(using p: SimParams): LendingConditions =
    val bsec               = in.w.bankingSector
    val nBanks             = in.banks.length
    val ccyb               = in.w.mechanisms.macropru.ccyb
    val rates              = in.banks.zip(bsec.configs).map((b, cfg) => Banking.lendingRate(b, cfg, in.s1.lendingBaseRate, in.w.gov.bondYield))
    val canLend            = (bankId: Int, amt: PLN) => Banking.canLend(in.banks(bankId), amt, rng, ccyb)
    val operationalSignals = OperationalSignals(
      sectorDemandMult = in.s4.sectorMults,
      sectorDemandPressure = in.s4.sectorDemandPressure,
      sectorHiringSignal = in.s4.sectorHiringSignal,
      operationalHiringSlack = in.s2.operationalHiringSlack,
    )
    val world              = in.w.copy(
      householdMarket = in.w.householdMarket.copy(
        marketWage = in.s2.newWage,
        reservationWage = in.s1.resWage,
      ),
    )
    LendingConditions(world, in.s1.m, operationalSignals, rates, canLend, nBanks)

  // ---- Phase 2: Per-firm processing ----

  /** Process each firm: technology decisions, financing splits (equity → bonds
    * → bank loans). Returns immutable per-firm outcomes and aggregate flows.
    */
  private def processFirms(
      firms: Vector[Firm.State],
      lending: LendingConditions,
      rng: RandomStream,
  )(using p: SimParams): FirmProcessingResult =
    val outcomes = firms.map: f =>
      val rate    = lending.rates(f.bankId.toInt)
      val canLend = (amt: PLN) => lending.bankCanLend(f.bankId.toInt, amt)
      val r       = Firm.process(f, lending.firmWorld, lending.executionMonth, lending.operationalSignals, rate, canLend, firms, rng)
      val fin     = splitFinancing(r)

      FirmOutcome(
        firm = fin.firm,
        flows = FirmFlows(
          tax = r.taxPaid,
          capex = r.capexSpent,
          techImp = r.techImports,
          newLoans = fin.bankLoan,
          equityIssuance = fin.equity,
          grossInvestment = r.grossInvestment,
          bondIssuance = fin.bonds,
          profitShifting = r.profitShiftCost,
          fdiRepatriation = r.fdiRepatriation,
          inventoryChange = r.inventoryChange,
          citEvasion = r.citEvasion,
          energyCost = r.energyCost,
          greenInvestment = r.greenInvestment,
          principalRepaid = r.principalRepaid,
        ),
        realizedPostTaxProfit = r.realizedPostTaxProfit,
        bankId = f.bankId,
        finalLoan = fin.bankLoan,
        bondAmt = fin.bonds,
        principalRepaid = r.principalRepaid,
      )

    val aggFlows = outcomes.foldLeft(FirmFlows.zero)((acc, o) => acc + o.flows)
    val bondMap  = outcomes.collect { case o if o.bondAmt > PLN.Zero => o.firm.id -> o.bondAmt }.toMap

    FirmProcessingResult(outcomes, aggFlows, bondMap)

  /** Split a firm's new loan into three channels: GPW equity → Catalyst bonds →
    * bank loan remainder.
    */
  private def splitFinancing(r: Firm.Result)(using p: SimParams): FinancingSplit =
    // Channel 1: GPW equity issuance (large firms only)
    val (afterEquityLoan, equityAmt, afterEquityFirm) =
      if r.newLoan > PLN.Zero &&
        Firm.workerCount(r.firm) >= p.equity.issuanceMinSize
      then
        val eq  = r.newLoan * p.equity.issuanceFrac
        val adj = r.newLoan - eq
        val f   = r.firm.copy(debt = r.firm.debt - eq, equityRaised = r.firm.equityRaised + eq)
        (adj, eq, f)
      else (r.newLoan, PLN.Zero, r.firm)

    // Channel 2: Catalyst corporate bonds (medium+ firms only)
    val (finalLoan, bondAmt, finalFirm) =
      if afterEquityLoan > PLN.Zero && Firm.workerCount(afterEquityFirm) >= p.corpBond.minSize then
        val ba  = afterEquityLoan * p.corpBond.issuanceFrac
        val adj = afterEquityLoan - ba
        val f   = afterEquityFirm.copy(debt = afterEquityFirm.debt - ba, bondDebt = afterEquityFirm.bondDebt + ba)
        (adj, ba, f)
      else (afterEquityLoan, PLN.Zero, afterEquityFirm)

    FinancingSplit(finalLoan, equityAmt, bondAmt, finalFirm)

  // ---- Phase 3: Bond absorption ----

  /** Apply Catalyst demand-side absorption constraint. Unsold bonds revert to
    * bank loans on the issuing firm's relationship bank.
    */
  private def applyBondAbsorption(
      result: FirmProcessingResult,
      w: World,
      banks: Vector[Banking.BankState],
  )(using p: SimParams): BondAbsorptionResult =
    val bankAgg            = Banking.aggregateFromBanks(banks)
    val absorption         = CorporateBondMarket
      .computeAbsorption(w.financial.corporateBonds, result.flows.bondIssuance, bankAgg.car, p.banking.minCar)
    val actualBondIssuance = result.flows.bondIssuance * absorption
    val revertShare        = Share.One - absorption

    val adjustedFirms =
      if revertShare > Share(BondRevertThreshold) then
        result.outcomes.map: o =>
          val ba = result.firmBondAmounts.getOrElse(o.firm.id, PLN.Zero)
          if ba > PLN.Zero then
            val revert = ba * revertShare
            o.firm.copy(bondDebt = o.firm.bondDebt - revert, debt = o.firm.debt + revert)
          else o.firm
      else result.outcomes.map(_.firm)

    val bondRevertLoans =
      if revertShare > Share(BondRevertThreshold) then result.flows.bondIssuance * revertShare
      else PLN.Zero

    BondAbsorptionResult(adjustedFirms, result.flows.newLoans + bondRevertLoans, absorption, actualBondIssuance)

  // ---- Phase 4: Intermediate market ----

  /** Run I-O intermediate goods market. Adjusts firm cash positions (zero-sum
    * transfers between sectors). Returns updated firms and total paid.
    */
  private def applyIntermediateMarket(
      firms: Vector[Firm.State],
      in: StepInput,
  )(using p: SimParams): (Vector[Firm.State], PLN) =

    val r = IntermediateMarket.process(
      IntermediateMarket.Input(
        firms = firms,
        sectorMults = in.s4.sectorMults,
        price = in.w.priceLevel,
        ioMatrix = p.io.matrix,
        columnSums = p.io.columnSums,
        scale = p.io.scale,
      ),
    )
    (r.firms, r.totalPaid)

  // ---- Phase 5: Labor market + immigration ----

  /** Run labor market: separate displaced workers, match unemployed to
    * vacancies (skill-ranked), update wages, process immigration flows.
    */
  private def processLaborMarket(
      ioFirms: Vector[Firm.State],
      in: StepInput,
      rng: RandomStream,
  )(using p: SimParams): (Vector[Household.State], Int) =
    val afterSep     = LaborMarket.separations(in.s3.updatedHouseholds, in.firms, ioFirms)
    val searchResult = LaborMarket.jobSearch(afterSep, ioFirms, in.s2.newWage, rng, in.s2.regionalWages)
    val postWages    = LaborMarket.updateWages(searchResult.households, ioFirms, in.s2.newWage)

    val finalHouseholds =
      val afterRemoval  = Immigration.removeReturnMigrants(postWages, in.s2.newImmig.monthlyOutflow)
      val startId       = afterRemoval.map(_.id.toInt).maxOption.getOrElse(-1) + 1
      val newImmigrants = Immigration.spawnImmigrants(in.s2.newImmig.monthlyInflow, startId, rng)
      afterRemoval ++ newImmigrants

    (finalHouseholds, searchResult.crossSectorHires)

  // ---- Phase 6: NPL and interest income ----

  /** Detect newly bankrupt firms, compute per-bank NPL losses and interest
    * income on pre-step debt stock.
    */
  private def computeNplAndInterest(
      preFirms: Vector[Firm.State],
      postFirms: Vector[Firm.State],
      lending: LendingConditions,
  )(using p: SimParams): NplResult =
    val prevAlive        = preFirms.filter(Firm.isAlive).map(_.id).toSet
    val newlyDead        = postFirms.filter(f => !Firm.isAlive(f) && prevAlive.contains(f.id))
    val nplNew           = PLN.fromRaw(newlyDead.map(_.debt.toLong).sum)
    val nplLoss          = nplNew * (Share.One - p.banking.loanRecovery)
    val totalBondDefault = PLN.fromRaw(newlyDead.map(_.bondDebt.toLong).sum)

    // Per-bank aggregation via groupMapReduce (pure, no mutable arrays)
    val emptyPln  = Vector.fill(lending.nBanks)(PLN.Zero)
    val emptyInts = Vector.fill(lending.nBanks)(0)

    val perBankNplDebt = newlyDead.foldLeft(emptyPln): (acc, f) =>
      acc.updated(f.bankId.toInt, acc(f.bankId.toInt) + f.debt)

    val perBankIntIncome = preFirms
      .filter(Firm.isAlive)
      .foldLeft(emptyPln): (acc, f) =>
        val interest = f.debt * lending.rates(f.bankId.toInt).monthly
        acc.updated(f.bankId.toInt, acc(f.bankId.toInt) + interest)

    val perBankWorkers = postFirms
      .filter(Firm.isAlive)
      .foldLeft(emptyInts): (acc, f) =>
        acc.updated(f.bankId.toInt, acc(f.bankId.toInt) + Firm.workerCount(f))

    NplResult(
      nplNew,
      nplLoss,
      totalBondDefault,
      newlyDead.length,
      perBankIntIncome.foldLeft(PLN.Zero)(_ + _),
      perBankNplDebt,
      perBankIntIncome,
      perBankWorkers,
    )

  // ---- Output assembly ----

  /** Assemble final StepOutput from phase results. Pure mapping, no
    * computation.
    */
  private def assembleOutput(
      fp: FirmProcessingResult,
      bonded: BondAbsorptionResult,
      ioFirms: Vector[Firm.State],
      totalIoPaid: PLN,
      households: Vector[Household.State],
      crossSectorHires: Int,
      npl: NplResult,
      in: StepInput,
      lending: LendingConditions,
      markupInflation: Rate,
  ): StepOutput =
    val flows = fp.flows

    // Derive per-bank vectors from immutable outcomes
    val emptyPln = Vector.fill(lending.nBanks)(PLN.Zero)

    val perBankNewLoans = fp.outcomes.foldLeft(emptyPln): (acc, o) =>
      acc.updated(o.bankId.toInt, acc(o.bankId.toInt) + o.finalLoan)

    // Add bond reversion amounts to per-bank loans
    val perBankNewLoansWithRevert =
      if bonded.corpBondAbsorption < Share.One then
        val revertShare = Share.One - bonded.corpBondAbsorption
        fp.outcomes.foldLeft(perBankNewLoans): (acc, o) =>
          val ba = fp.firmBondAmounts.getOrElse(o.firm.id, PLN.Zero)
          if ba > PLN.Zero then acc.updated(o.bankId.toInt, acc(o.bankId.toInt) + ba * revertShare)
          else acc
      else perBankNewLoans

    val perBankFirmPrincipal = fp.outcomes.foldLeft(emptyPln): (acc, o) =>
      acc.updated(o.bankId.toInt, acc(o.bankId.toInt) + o.principalRepaid)

    StepOutput(
      ioFirms = ioFirms,
      households = households,
      sumTax = flows.tax,
      sumCapex = flows.capex,
      sumTechImp = flows.techImp,
      sumNewLoans = bonded.sumNewLoans,
      sumEquityIssuance = flows.equityIssuance,
      sumGrossInvestment = flows.grossInvestment,
      sumBondIssuance = flows.bondIssuance,
      sumProfitShifting = flows.profitShifting,
      sumFdiRepatriation = flows.fdiRepatriation,
      sumInventoryChange = flows.inventoryChange,
      sumCitEvasion = flows.citEvasion,
      sumEnergyCost = flows.energyCost,
      sumGreenInvestment = flows.greenInvestment,
      totalIoPaid = totalIoPaid,
      nplNew = npl.nplNew,
      nplLoss = npl.nplLoss,
      totalBondDefault = npl.totalBondDefault,
      firmDeaths = npl.firmDeaths,
      intIncome = npl.intIncome,
      corpBondAbsorption = bonded.corpBondAbsorption,
      actualBondIssuance = bonded.actualBondIssuance,
      netMigration = in.s2.newImmig.monthlyInflow - in.s2.newImmig.monthlyOutflow,
      perBankNewLoans = perBankNewLoansWithRevert,
      sumFirmPrincipal = flows.principalRepaid,
      perBankFirmPrincipal = perBankFirmPrincipal,
      perBankNplDebt = npl.perBankNplDebt,
      perBankIntIncome = npl.perBankIntIncome,
      perBankWorkers = npl.perBankWorkers,
      lendingRates = lending.rates,
      postFirmCrossSectorHires = crossSectorHires,
      markupInflation = markupInflation,
      sumRealizedPostTaxProfit = fp.outcomes.foldLeft(PLN.Zero)(_ + _.realizedPostTaxProfit),
      sumStateOwnedPostTaxProfit = fp.outcomes.filter(_.firm.stateOwned).foldLeft(PLN.Zero)((acc, o) => acc + o.realizedPostTaxProfit),
    )
