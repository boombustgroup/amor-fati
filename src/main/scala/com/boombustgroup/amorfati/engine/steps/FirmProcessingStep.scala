package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.{CalvoPricing, CorporateBondMarket, IntermediateMarket, LaborMarket}
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

import scala.util.Random

/** Monthly firm sector cycle: each firm decides on investment and automation,
  * finances CAPEX through bank credit / equity / corporate bonds, trades
  * intermediate goods across sectors, and the labor market clears — displaced
  * workers search for jobs, wages adjust, immigrants arrive. Bankrupt firms
  * generate credit losses allocated to their relationship bank.
  */
object FirmProcessingStep:

  // ---- Calibration constants ----
  private val BondRevertThreshold = 0.001 // minimum revert ratio to trigger bond-to-loan reversion
  private val MonthsPerYear       = 12.0  // annual-to-monthly rate conversion

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

  // ---- Public I/O types ----

  case class Input(
      w: World,                            // current world state
      firms: Vector[Firm.State],           // pre-step firm population
      households: Vector[Household.State], // pre-step household population
      s1: FiscalConstraintStep.Output,     // fiscal constraint (lending base rate, reservation wage)
      s2: LaborDemographicsStep.Output,    // labor/demographics (new wage, immigration params)
      s3: HouseholdIncomeStep.Output,      // household income (updated households post-income)
      s4: DemandStep.Output,               // demand (sector demand multipliers)
  )

  case class Output(
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
      perBankNplDebt: Vector[Double],      // NPL debt by bank index
      perBankIntIncome: Vector[Double],    // interest income by bank index
      perBankWorkers: Vector[Int],         // worker count by bank index
      lendingRates: Vector[Double],        // per-bank lending rates
      postFirmCrossSectorHires: Int,       // cross-sector hires in labor matching
      markupInflation: Rate,               // Calvo: annualized revenue-weighted avg markup change
  )

  // ---- Internal phase result types ----

  /** Per-bank lending rates and credit approval, shared across phases. */
  private case class LendingConditions(
      macro4firms: World,                 // world with updated demand mults + wages
      rates: Vector[Rate],                // per-bank lending rates
      lendingRates: Vector[Double],       // rates as Double (for Output)
      bankCanLend: (Int, PLN) => Boolean, // credit approval: (bankId, amount) => approved?
      nBanks: Int,                        // number of banks in multi-bank system
  )

  /** Per-firm processing outcome — immutable, one per firm. */
  private case class FirmOutcome(
      firm: Firm.State,    // updated firm state after decision + financing
      flows: FirmFlows,    // monetary flows from this firm
      bankId: BankId,      // relationship bank (for per-bank aggregation)
      finalLoan: PLN,      // bank loan after equity/bond splits
      bondAmt: PLN,        // corporate bond issuance (pre-absorption)
      principalRepaid: PLN, // scheduled loan principal repaid this month
  )

  /** Result of per-firm processing (phase 2). */
  private case class FirmProcessingResult(
      outcomes: Vector[FirmOutcome],       // per-firm immutable outcomes
      flows: FirmFlows,                    // aggregate flows (monoid sum)
      firmBondAmounts: Map[FirmId, Double], // per-firm bond issuance for reversion
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
      nplNew: PLN,                      // new NPL volume from bankruptcies
      nplLoss: PLN,                     // NPL loss net of recovery
      totalBondDefault: PLN,            // bond default from bankrupt firms
      firmDeaths: Int,                  // count of newly bankrupt firms
      intIncome: PLN,                   // aggregate bank interest income
      perBankNplDebt: Vector[Double],   // NPL debt by bank index
      perBankIntIncome: Vector[Double], // interest income by bank index
      perBankWorkers: Vector[Int],      // worker count by bank index
  )

  // ---- Entry point ----

  def run(in: Input, rng: Random)(using p: SimParams): Output =
    val lending                             = prepareLending(in, rng)
    val fp                                  = processFirms(in.firms, lending, rng)
    val bonded                              = applyBondAbsorption(fp, in.w)
    val (ioFirms, totalIoPaid)              = applyIntermediateMarket(bonded.firms, in)
    // Calvo staggered pricing: per-firm markup update
    val calvoFirms                          = ioFirms.map: f =>
      val sectorMult = in.s4.sectorMults(f.sector.toInt)
      val calvo      = CalvoPricing.updateFirmMarkup(f.markup, sectorMult, in.s2.wageGrowth.toDouble, rng)
      f.copy(markup = calvo.newMarkup)
    val (finalHouseholds, crossSectorHires) = processLaborMarket(calvoFirms, in, rng)
    val npl                                 = computeNplAndInterest(in.firms, calvoFirms, lending)
    val markupInfl                          = Rate(CalvoPricing.aggregateMarkupInflation(calvoFirms, ioFirms)).annualize
    assembleOutput(fp, bonded, calvoFirms, totalIoPaid, finalHouseholds, crossSectorHires, npl, in, lending, markupInfl)

  // ---- Phase 1: Lending conditions ----

  /** Prepare per-bank rates, lending functions, and world snapshot with updated
    * demand multipliers and wages for firm decision-making.
    */
  private def prepareLending(in: Input, rng: Random)(using p: SimParams): LendingConditions =
    val bsec    = in.w.bankingSector
    val nBanks  = bsec.banks.length
    val ccyb    = in.w.mechanisms.macropru.ccyb
    val rates   = bsec.banks.zip(bsec.configs).map((b, cfg) => Banking.lendingRate(b, cfg, in.s1.lendingBaseRate, in.w.gov.bondYield))
    val canLend = (bankId: Int, amt: PLN) => Banking.canLend(bsec.banks(bankId), amt, rng, ccyb)
    val world   = in.w.copy(
      month = in.s1.m,
      flows = in.w.flows.copy(sectorDemandMult = in.s4.sectorMults),
      hhAgg = in.w.hhAgg.copy(marketWage = in.s2.newWage, reservationWage = in.s1.resWage),
    )
    LendingConditions(world, rates, rates.map(_.toDouble), canLend, nBanks)

  // ---- Phase 2: Per-firm processing ----

  /** Process each firm: technology decisions, financing splits (equity → bonds
    * → bank loans). Returns immutable per-firm outcomes and aggregate flows.
    */
  private def processFirms(
      firms: Vector[Firm.State],
      lending: LendingConditions,
      rng: Random,
  )(using p: SimParams): FirmProcessingResult =
    val outcomes = firms.map: f =>
      val rate    = lending.rates(f.bankId.toInt)
      val canLend = (amt: PLN) => lending.bankCanLend(f.bankId.toInt, amt)
      val r       = Firm.process(f, lending.macro4firms, rate, canLend, firms, rng)
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
        bankId = f.bankId,
        finalLoan = fin.bankLoan,
        bondAmt = fin.bonds,
        principalRepaid = r.principalRepaid,
      )

    val aggFlows = outcomes.foldLeft(FirmFlows.zero)((acc, o) => acc + o.flows)
    val bondMap  = outcomes.collect { case o if o.bondAmt > PLN.Zero => o.firm.id -> o.bondAmt.toDouble }.toMap

    FirmProcessingResult(outcomes, aggFlows, bondMap)

  /** Split a firm's new loan into three channels: GPW equity → Catalyst bonds →
    * bank loan remainder.
    */
  private def splitFinancing(r: Firm.Result)(using p: SimParams): FinancingSplit =
    // Channel 1: GPW equity issuance (large firms only)
    val (afterEquityLoan, equityAmt, afterEquityFirm) =
      if p.flags.gpw && p.flags.gpwEquityIssuance && r.newLoan > PLN.Zero &&
        Firm.workerCount(r.firm) >= p.equity.issuanceMinSize
      then
        val eq  = r.newLoan * p.equity.issuanceFrac.toDouble
        val adj = r.newLoan - eq
        val f   = r.firm.copy(debt = r.firm.debt - eq, equityRaised = r.firm.equityRaised + eq)
        (adj, eq, f)
      else (r.newLoan, PLN.Zero, r.firm)

    // Channel 2: Catalyst corporate bonds (medium+ firms only)
    val (finalLoan, bondAmt, finalFirm) =
      if afterEquityLoan > PLN.Zero && Firm.workerCount(afterEquityFirm) >= p.corpBond.minSize then
        val ba  = afterEquityLoan * p.corpBond.issuanceFrac.toDouble
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
  )(using p: SimParams): BondAbsorptionResult =
    val absorption         = CorporateBondMarket
      .computeAbsorption(w.financial.corporateBonds, result.flows.bondIssuance, w.bank.car, p.banking.minCar)
      .toDouble
    val actualBondIssuance = result.flows.bondIssuance * absorption
    val revertRatio        = 1.0 - absorption

    val adjustedFirms =
      if revertRatio > BondRevertThreshold then
        result.outcomes.map: o =>
          val ba = result.firmBondAmounts.getOrElse(o.firm.id, 0.0)
          if ba > 0 then
            val revert = ba * revertRatio
            o.firm.copy(bondDebt = o.firm.bondDebt - PLN(revert), debt = o.firm.debt + PLN(revert))
          else o.firm
      else result.outcomes.map(_.firm)

    val bondRevertLoans =
      if revertRatio > BondRevertThreshold then result.flows.bondIssuance * revertRatio
      else PLN.Zero

    BondAbsorptionResult(adjustedFirms, result.flows.newLoans + bondRevertLoans, Share(absorption), actualBondIssuance)

  // ---- Phase 4: Intermediate market ----

  /** Run I-O intermediate goods market. Adjusts firm cash positions (zero-sum
    * transfers between sectors). Returns updated firms and total paid.
    */
  private def applyIntermediateMarket(
      firms: Vector[Firm.State],
      in: Input,
  )(using p: SimParams): (Vector[Firm.State], PLN) =
    if p.flags.io then
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
    else (firms, PLN.Zero)

  // ---- Phase 5: Labor market + immigration ----

  /** Run labor market: separate displaced workers, match unemployed to
    * vacancies (skill-ranked), update wages, process immigration flows.
    */
  private def processLaborMarket(
      ioFirms: Vector[Firm.State],
      in: Input,
      rng: Random,
  )(using p: SimParams): (Vector[Household.State], Int) =
    val afterSep     = LaborMarket.separations(in.s3.updatedHouseholds, in.firms, ioFirms)
    val searchResult = LaborMarket.jobSearch(afterSep, ioFirms, in.s2.newWage, rng, in.s2.regionalWages)
    val postWages    = LaborMarket.updateWages(searchResult.households, ioFirms, in.s2.newWage)

    val finalHouseholds =
      if p.flags.immigration then
        val afterRemoval  = Immigration.removeReturnMigrants(postWages, in.s2.newImmig.monthlyOutflow)
        val startId       = afterRemoval.map(_.id.toInt).maxOption.getOrElse(-1) + 1
        val newImmigrants = Immigration.spawnImmigrants(in.s2.newImmig.monthlyInflow, startId, rng)
        afterRemoval ++ newImmigrants
      else postWages

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
    val nplNew           = PLN(newlyDead.kahanSumBy(_.debt.toDouble))
    val nplLoss          = nplNew * (1.0 - p.banking.loanRecovery.toDouble)
    val totalBondDefault = PLN(newlyDead.kahanSumBy(_.bondDebt.toDouble))

    // Per-bank aggregation via groupMapReduce (pure, no mutable arrays)
    val emptyDoubles = Vector.fill(lending.nBanks)(0.0)
    val emptyInts    = Vector.fill(lending.nBanks)(0)

    val perBankNplDebt = newlyDead.foldLeft(emptyDoubles): (acc, f) =>
      acc.updated(f.bankId.toInt, acc(f.bankId.toInt) + f.debt.toDouble)

    val perBankIntIncome = preFirms
      .filter(Firm.isAlive)
      .foldLeft(emptyDoubles): (acc, f) =>
        val interest = f.debt.toDouble * lending.rates(f.bankId.toInt).toDouble / MonthsPerYear
        acc.updated(f.bankId.toInt, acc(f.bankId.toInt) + interest)

    val perBankWorkers = postFirms
      .filter(Firm.isAlive)
      .foldLeft(emptyInts): (acc, f) =>
        acc.updated(f.bankId.toInt, acc(f.bankId.toInt) + Firm.workerCount(f))

    NplResult(nplNew, nplLoss, totalBondDefault, newlyDead.length, PLN(perBankIntIncome.kahanSum), perBankNplDebt, perBankIntIncome, perBankWorkers)

  // ---- Output assembly ----

  /** Assemble final Output from phase results. Pure mapping, no computation. */
  private def assembleOutput(
      fp: FirmProcessingResult,
      bonded: BondAbsorptionResult,
      ioFirms: Vector[Firm.State],
      totalIoPaid: PLN,
      households: Vector[Household.State],
      crossSectorHires: Int,
      npl: NplResult,
      in: Input,
      lending: LendingConditions,
      markupInflation: Rate,
  ): Output =
    val flows = fp.flows

    // Derive per-bank vectors from immutable outcomes
    val emptyPln = Vector.fill(lending.nBanks)(PLN.Zero)

    val perBankNewLoans = fp.outcomes.foldLeft(emptyPln): (acc, o) =>
      acc.updated(o.bankId.toInt, acc(o.bankId.toInt) + o.finalLoan)

    // Add bond reversion amounts to per-bank loans
    val perBankNewLoansWithRevert =
      if bonded.corpBondAbsorption < Share.One then
        val revertRatio = 1.0 - bonded.corpBondAbsorption.toDouble
        fp.outcomes.foldLeft(perBankNewLoans): (acc, o) =>
          val ba = fp.firmBondAmounts.getOrElse(o.firm.id, 0.0)
          if ba > 0 then acc.updated(o.bankId.toInt, acc(o.bankId.toInt) + PLN(ba * revertRatio))
          else acc
      else perBankNewLoans

    val perBankFirmPrincipal = fp.outcomes.foldLeft(emptyPln): (acc, o) =>
      acc.updated(o.bankId.toInt, acc(o.bankId.toInt) + o.principalRepaid)

    Output(
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
      lendingRates = lending.lendingRates,
      postFirmCrossSectorHires = crossSectorHires,
      markupInflation = markupInflation,
    )
