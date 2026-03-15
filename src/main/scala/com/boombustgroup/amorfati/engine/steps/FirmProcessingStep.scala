package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, IntermediateMarket, LaborMarket}
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

import scala.util.Random

/** Monthly firm processing pipeline: production decisions, financing (bank
  * loans, equity issuance, corporate bonds), intermediate-goods trade via the
  * I-O market, labor market matching (separations, job search, wage updates),
  * and immigration flows.
  *
  * Decomposed into six pure phases:
  *   1. `buildContext` — rates, lending functions, world snapshot for firms
  *   2. `processFirms` — per-firm decisions, financing splits, flow
  *      accumulation
  *   3. `applyBondAbsorption` — Catalyst demand constraint, unsold bonds → bank
  *      loans
  *   4. `applyIntermediateMarket` — I-O intermediate goods trade
  *   5. `processLaborMarket` — separations, job search, wages, immigration
  *   6. `computeNplAndInterest` — dead firm detection, per-bank NPL and
  *      interest
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
      tax = tax + o.tax,
      capex = capex + o.capex,
      techImp = techImp + o.techImp,
      newLoans = newLoans + o.newLoans,
      equityIssuance = equityIssuance + o.equityIssuance,
      grossInvestment = grossInvestment + o.grossInvestment,
      bondIssuance = bondIssuance + o.bondIssuance,
      profitShifting = profitShifting + o.profitShifting,
      fdiRepatriation = fdiRepatriation + o.fdiRepatriation,
      inventoryChange = inventoryChange + o.inventoryChange,
      citEvasion = citEvasion + o.citEvasion,
      energyCost = energyCost + o.energyCost,
      greenInvestment = greenInvestment + o.greenInvestment,
      principalRepaid = principalRepaid + o.principalRepaid,
    )

  private object FirmFlows:
    val zero: FirmFlows = FirmFlows(
      tax = PLN.Zero,
      capex = PLN.Zero,
      techImp = PLN.Zero,
      newLoans = PLN.Zero,
      equityIssuance = PLN.Zero,
      grossInvestment = PLN.Zero,
      bondIssuance = PLN.Zero,
      profitShifting = PLN.Zero,
      fdiRepatriation = PLN.Zero,
      inventoryChange = PLN.Zero,
      citEvasion = PLN.Zero,
      energyCost = PLN.Zero,
      greenInvestment = PLN.Zero,
      principalRepaid = PLN.Zero,
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
      corpBondAbsorption: Ratio,           // Catalyst absorption ratio (0-1)
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
  )

  // ---- Internal phase result types ----

  /** Context shared across phases — rates, lending functions, world snapshot.
    */
  private case class FirmContext(
      macro4firms: World,                 // world with updated demand mults + wages
      rates: Vector[Rate],                // per-bank lending rates
      lendingRates: Vector[Double],       // rates as Double (for Output)
      bankCanLend: (Int, PLN) => Boolean, // credit approval function
      nBanks: Int,                        // number of banks
      currentCcyb: Rate,                  // current countercyclical buffer
  )

  /** Result of per-firm processing (phase 2). */
  private case class FirmProcessingResult(
      firms: Vector[Firm.State],                                       // firms after decisions + financing
      flows: FirmFlows,                                                // aggregate monetary flows
      firmBondAmounts: scala.collection.immutable.Map[FirmId, Double], // per-firm bond issuance
      perBankNewLoans: Array[Double],                                  // mutable accumulator (tight loop)
      perBankFirmPrincipal: Array[Double],                             // mutable accumulator (tight loop)
  )

  /** Result of bond absorption (phase 3). */
  private case class BondAbsorptionResult(
      firms: Vector[Firm.State],     // firms after reversion
      sumNewLoans: PLN,              // total new loans incl. reversion
      corpBondAbsorption: Ratio,     // absorption ratio (0-1)
      actualBondIssuance: PLN,       // issued after absorption constraint
      perBankNewLoans: Array[Double], // updated with reversion
  )

  /** Result of NPL and interest computation (phase 6). */
  private case class NplResult(
      nplNew: PLN,                      // new NPL volume
      nplLoss: PLN,                     // NPL loss net of recovery
      totalBondDefault: PLN,            // bond default from bankrupt firms
      firmDeaths: Int,                  // count of newly bankrupt firms
      intIncome: PLN,                   // aggregate bank interest income
      perBankNplDebt: Vector[Double],   // NPL by bank index
      perBankIntIncome: Vector[Double], // interest income by bank index
      perBankWorkers: Vector[Int],      // worker count by bank index
  )

  // ---- Entry point ----

  def run(in: Input, rng: Random)(using p: SimParams): Output =
    val ctx                                 = buildContext(in, rng)
    val fp                                  = processFirms(in.firms, ctx, rng)
    val bonded                              = applyBondAbsorption(fp, in.w)
    val (ioFirms, totalIoPaid)              = applyIntermediateMarket(bonded.firms, in)
    val (finalHouseholds, crossSectorHires) = processLaborMarket(ioFirms, in, rng)
    val npl                                 = computeNplAndInterest(in.firms, ioFirms, ctx)
    assembleOutput(fp, bonded, ioFirms, totalIoPaid, finalHouseholds, crossSectorHires, npl, in, ctx)

  // ---- Phase 1: Build context ----

  /** Prepare shared context: per-bank rates, lending functions, world snapshot
    * with updated demand multipliers and wages for firm decision-making.
    */
  private def buildContext(in: Input, rng: Random)(using p: SimParams): FirmContext =
    val bsec        = in.w.bankingSector
    val nBanks      = bsec.banks.length
    val ccyb        = in.w.mechanisms.macropru.ccyb
    val rates       = bsec.banks.zip(bsec.configs).map((b, cfg) => Banking.lendingRate(b, cfg, in.s1.lendingBaseRate))
    val canLend     = (bankId: Int, amt: PLN) => Banking.canLend(bsec.banks(bankId), amt, rng, ccyb)
    val macro4firms = in.w.copy(
      month = in.s1.m,
      flows = in.w.flows.copy(sectorDemandMult = in.s4.sectorMults),
      hhAgg = in.w.hhAgg.copy(marketWage = in.s2.newWage, reservationWage = in.s1.resWage),
    )
    FirmContext(macro4firms, rates, rates.map(_.toDouble), canLend, nBanks, ccyb)

  // ---- Phase 2: Per-firm processing ----

  /** Process each firm: technology decisions, financing splits (equity → bonds
    * → bank loans), and per-bank accumulation. Mutable arrays used for per-bank
    * accumulators (tight 10K-firm loop with clear boundary).
    */
  private def processFirms(firms: Vector[Firm.State], ctx: FirmContext, rng: Random)(using p: SimParams): FirmProcessingResult =
    val perBankNewLoans      = new Array[Double](ctx.nBanks)
    val perBankFirmPrincipal = new Array[Double](ctx.nBanks)
    val firmBondAmounts      = scala.collection.mutable.HashMap.empty[FirmId, Double]

    val pairs = firms.map: f =>
      val r                                            = Firm.process(f, ctx.macro4firms, ctx.rates(f.bankId.toInt), amt => ctx.bankCanLend(f.bankId.toInt, amt), firms, rng)
      val (finalLoan, equityAmt, bondAmt, updatedFirm) = splitFinancing(r)

      if bondAmt > PLN.Zero then firmBondAmounts(f.id) = bondAmt.toDouble
      perBankNewLoans(f.bankId.toInt) += finalLoan.toDouble
      perBankFirmPrincipal(f.bankId.toInt) += r.principalRepaid.toDouble

      val flows = FirmFlows(
        tax = r.taxPaid,
        capex = r.capexSpent,
        techImp = r.techImports,
        newLoans = finalLoan,
        equityIssuance = equityAmt,
        grossInvestment = r.grossInvestment,
        bondIssuance = bondAmt,
        profitShifting = r.profitShiftCost,
        fdiRepatriation = r.fdiRepatriation,
        inventoryChange = r.inventoryChange,
        citEvasion = r.citEvasion,
        energyCost = r.energyCost,
        greenInvestment = r.greenInvestment,
        principalRepaid = r.principalRepaid,
      )
      (updatedFirm, flows)

    val (newFirms, flowsPerFirm) = pairs.unzip
    FirmProcessingResult(newFirms, flowsPerFirm.foldLeft(FirmFlows.zero)(_ + _), firmBondAmounts.toMap, perBankNewLoans, perBankFirmPrincipal)

  /** Split a firm's new loan into three channels: GPW equity → Catalyst bonds →
    * bank loan remainder. Returns (finalBankLoan, equityIssued, bondIssued,
    * updatedFirm).
    */
  private def splitFinancing(r: Firm.Result)(using p: SimParams): (PLN, PLN, PLN, Firm.State) =
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

    (finalLoan, equityAmt, bondAmt, finalFirm)

  // ---- Phase 3: Bond absorption ----

  /** Apply Catalyst demand-side absorption constraint. Unsold bonds revert to
    * bank loans on the issuing firm's relationship bank.
    */
  private def applyBondAbsorption(result: FirmProcessingResult, w: World)(using p: SimParams): BondAbsorptionResult =
    val absorption         = CorporateBondMarket
      .computeAbsorption(w.financial.corporateBonds, result.flows.bondIssuance, w.bank.car, p.banking.minCar)
      .toDouble
    val actualBondIssuance = result.flows.bondIssuance * absorption
    val revertRatio        = 1.0 - absorption

    val (adjustedFirms, bondRevertLoans) =
      if revertRatio > BondRevertThreshold then
        val reverted = result.firms.map: f =>
          val ba = result.firmBondAmounts.getOrElse(f.id, 0.0)
          if ba > 0 then
            val revert = ba * revertRatio
            f.copy(bondDebt = f.bondDebt - PLN(revert), debt = f.debt + PLN(revert))
          else f
        // Update per-bank loan accumulators with reverted amounts
        for (fid, ba) <- result.firmBondAmounts do
          val revert = ba * revertRatio
          reverted.find(_.id == fid).foreach(af => result.perBankNewLoans(af.bankId.toInt) += revert)
        (reverted, result.flows.bondIssuance * revertRatio)
      else (result.firms, PLN.Zero)

    BondAbsorptionResult(
      adjustedFirms,
      result.flows.newLoans + bondRevertLoans,
      Ratio(absorption),
      actualBondIssuance,
      result.perBankNewLoans,
    )

  // ---- Phase 4: Intermediate market ----

  /** Run I-O intermediate goods market. Adjusts firm cash positions (zero-sum
    * transfers between sectors). Returns updated firms and total paid.
    */
  private def applyIntermediateMarket(firms: Vector[Firm.State], in: Input)(using p: SimParams): (Vector[Firm.State], PLN) =
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
    val searchResult = LaborMarket.jobSearch(afterSep, ioFirms, in.s2.newWage, rng)
    val postWages    = LaborMarket.updateWages(searchResult.households, in.s2.newWage)

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
      ctx: FirmContext,
  )(using p: SimParams): NplResult =
    val prevAlive        = preFirms.filter(Firm.isAlive).map(_.id).toSet
    val newlyDead        = postFirms.filter(f => !Firm.isAlive(f) && prevAlive.contains(f.id))
    val nplNew           = PLN(newlyDead.kahanSumBy(_.debt.toDouble))
    val nplLoss          = nplNew * (1.0 - p.banking.loanRecovery.toDouble)
    val totalBondDefault = PLN(newlyDead.kahanSumBy(_.bondDebt.toDouble))

    val perBankNplDebt   = new Array[Double](ctx.nBanks)
    val perBankIntIncome = new Array[Double](ctx.nBanks)
    val perBankWorkers   = new Array[Int](ctx.nBanks)

    for f <- newlyDead do perBankNplDebt(f.bankId.toInt) += f.debt.toDouble
    for f <- preFirms if Firm.isAlive(f) do perBankIntIncome(f.bankId.toInt) += f.debt.toDouble * ctx.rates(f.bankId.toInt).toDouble / MonthsPerYear
    for f <- postFirms if Firm.isAlive(f) do perBankWorkers(f.bankId.toInt) += Firm.workerCount(f)

    NplResult(
      nplNew,
      nplLoss,
      totalBondDefault,
      newlyDead.length,
      PLN(perBankIntIncome.kahanSum),
      perBankNplDebt.toVector,
      perBankIntIncome.toVector,
      perBankWorkers.toVector,
    )

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
      ctx: FirmContext,
  ): Output =
    val flows = fp.flows
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
      perBankNewLoans = bonded.perBankNewLoans.toVector.map(PLN(_)),
      sumFirmPrincipal = flows.principalRepaid,
      perBankFirmPrincipal = fp.perBankFirmPrincipal.toVector.map(PLN(_)),
      perBankNplDebt = npl.perBankNplDebt,
      perBankIntIncome = npl.perBankIntIncome,
      perBankWorkers = npl.perBankWorkers,
      lendingRates = ctx.lendingRates,
      postFirmCrossSectorHires = crossSectorHires,
    )
