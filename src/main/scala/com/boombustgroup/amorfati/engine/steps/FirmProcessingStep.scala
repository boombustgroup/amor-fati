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
  * Each firm independently decides on technology upgrades, investment, and
  * borrowing (see `Firm.process`). Loan demand is split across three channels —
  * bank credit, GPW equity issuance (KNF 2024 thresholds), and Catalyst
  * corporate bonds — with a demand-side absorption constraint on bond issuance
  * that reverts unsold bonds back to bank loans.
  *
  * Bankrupt firms generate NPL losses allocated to their relationship bank.
  * Interest income is computed on the pre-step debt stock (annual rate / 12).
  */
object FirmProcessingStep:

  // ---- Calibration constants ----
  private val BondRevertThreshold = 0.001 // minimum revert ratio to trigger bond-to-loan reversion
  private val MonthsPerYear       = 12.0  // annual-to-monthly rate conversion

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

  def run(in: Input, rng: Random)(using p: SimParams): Output =
    val bsec                 = in.w.bankingSector
    val nBanks               = bsec.banks.length
    val perBankNewLoans      = new Array[Double](nBanks)
    val perBankFirmPrincipal = new Array[Double](nBanks)
    val perBankNplDebt       = new Array[Double](nBanks)
    val perBankIntIncome     = new Array[Double](nBanks)
    val perBankWorkers       = new Array[Int](nBanks)

    val currentCcyb                          = in.w.mechanisms.macropru.ccyb
    val rates                                = bsec.banks.zip(bsec.configs).map((b, cfg) => Banking.lendingRate(b, cfg, in.s1.lendingBaseRate))
    val getFirmRate: Int => Rate             = (bankId: Int) => rates(bankId)
    val bankCanLendFn: (Int, PLN) => Boolean =
      (bankId: Int, amt: PLN) => Banking.canLend(bsec.banks(bankId), amt, rng, currentCcyb)

    val lendingRates = rates.map(_.toDouble)

    val macro4firms = in.w.copy(
      month = in.s1.m,
      flows = in.w.flows.copy(sectorDemandMult = in.s4.sectorMults),
      hhAgg = in.w.hhAgg.copy(marketWage = in.s2.newWage, reservationWage = in.s1.resWage),
    )

    val firmBondAmounts = scala.collection.mutable.HashMap.empty[FirmId, Double]

    val pairs = in.firms.map: f =>
      val firmRate                    = getFirmRate(f.bankId.toInt)
      val firmCanLend: PLN => Boolean = amt => bankCanLendFn(f.bankId.toInt, amt)
      val r                           = Firm.process(f, macro4firms, firmRate, firmCanLend, in.firms, rng)

      val (actualLoan, equityAmt, updatedFirm) =
        if p.flags.gpw && p.flags.gpwEquityIssuance && r.newLoan > PLN.Zero &&
          Firm.workerCount(r.firm) >= p.equity.issuanceMinSize
        then
          val eqAmt   = r.newLoan * p.equity.issuanceFrac.toDouble
          val adjLoan = r.newLoan - eqAmt
          val f2      = r.firm.copy(
            debt = r.firm.debt - eqAmt,
            equityRaised = r.firm.equityRaised + eqAmt,
          )
          (adjLoan, eqAmt, f2)
        else (r.newLoan, PLN.Zero, r.firm)

      val (finalLoan, bondAmt, bondUpdatedFirm) =
        if actualLoan > PLN.Zero && Firm.workerCount(updatedFirm) >= p.corpBond.minSize then
          val ba      = actualLoan * p.corpBond.issuanceFrac.toDouble
          val adjLoan = actualLoan - ba
          val f3      = updatedFirm.copy(
            debt = updatedFirm.debt - ba,
            bondDebt = updatedFirm.bondDebt + ba,
          )
          (adjLoan, ba, f3)
        else (actualLoan, PLN.Zero, updatedFirm)

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
      (bondUpdatedFirm, flows)

    val (newFirms, flowsPerFirm) = pairs.unzip
    val ff                       = flowsPerFirm.foldLeft(FirmFlows.zero)(_ + _)

    val corpBondAbsorption =
      CorporateBondMarket
        .computeAbsorption(
          in.w.financial.corporateBonds,
          ff.bondIssuance,
          in.w.bank.car,
          p.banking.minCar,
        )
        .toDouble
    val actualBondIssuance = ff.bondIssuance * corpBondAbsorption
    val revertRatio        = 1.0 - corpBondAbsorption
    val adjustedFirms      =
      if revertRatio > BondRevertThreshold then
        newFirms.map: f =>
          val ba = firmBondAmounts.getOrElse(f.id, 0.0)
          if ba > 0 then
            val revert = ba * revertRatio
            f.copy(bondDebt = f.bondDebt - PLN(revert), debt = f.debt + PLN(revert))
          else f
      else newFirms
    val bondRevertLoans    =
      if revertRatio > BondRevertThreshold then
        for (fid, ba) <- firmBondAmounts do
          val revert = ba * revertRatio
          adjustedFirms.find(_.id == fid).foreach(af => perBankNewLoans(af.bankId.toInt) += revert)
        ff.bondIssuance * revertRatio
      else PLN.Zero
    val sumNewLoans        = ff.newLoans + bondRevertLoans

    for f <- adjustedFirms if Firm.isAlive(f) do perBankWorkers(f.bankId.toInt) += Firm.workerCount(f)

    val (ioFirms, totalIoPaid) = if p.flags.io then
      val r = IntermediateMarket.process(
        IntermediateMarket.Input(
          firms = adjustedFirms,
          sectorMults = in.s4.sectorMults,
          price = in.w.priceLevel,
          ioMatrix = p.io.matrix,
          columnSums = p.io.columnSums,
          scale = p.io.scale,
        ),
      )
      (r.firms, r.totalPaid.toDouble)
    else (adjustedFirms, 0.0)

    val afterSep                 = LaborMarket.separations(in.s3.updatedHouseholds, in.firms, ioFirms)
    val searchResult             = LaborMarket.jobSearch(afterSep, ioFirms, in.s2.newWage, rng)
    val postFirmCrossSectorHires = searchResult.crossSectorHires
    val preMigrationHouseholds   = LaborMarket.updateWages(searchResult.households, in.s2.newWage)

    val finalHouseholds =
      if p.flags.immigration then
        val afterRemoval  = Immigration.removeReturnMigrants(preMigrationHouseholds, in.s2.newImmig.monthlyOutflow)
        val startId       = afterRemoval.map(_.id.toInt).maxOption.getOrElse(-1) + 1
        val newImmigrants = Immigration.spawnImmigrants(in.s2.newImmig.monthlyInflow, startId, rng)
        afterRemoval ++ newImmigrants
      else preMigrationHouseholds

    val prevAlive        = in.firms.filter(Firm.isAlive).map(_.id).toSet
    val newlyDead        = ioFirms.filter(f => !Firm.isAlive(f) && prevAlive.contains(f.id))
    val firmDeaths       = newlyDead.length
    val nplNew           = newlyDead.kahanSumBy(_.debt.toDouble)
    val nplLoss          = nplNew * (1.0 - p.banking.loanRecovery.toDouble)
    val totalBondDefault = newlyDead.kahanSumBy(_.bondDebt.toDouble)

    for f <- newlyDead do perBankNplDebt(f.bankId.toInt) += f.debt.toDouble

    for f <- in.firms if Firm.isAlive(f) do perBankIntIncome(f.bankId.toInt) += f.debt.toDouble * rates(f.bankId.toInt).toDouble / MonthsPerYear

    val intIncome    = perBankIntIncome.kahanSum
    val netMigration = in.s2.newImmig.monthlyInflow - in.s2.newImmig.monthlyOutflow

    Output(
      ioFirms = ioFirms,
      households = finalHouseholds,
      sumTax = ff.tax,
      sumCapex = ff.capex,
      sumTechImp = ff.techImp,
      sumNewLoans = sumNewLoans,
      sumEquityIssuance = ff.equityIssuance,
      sumGrossInvestment = ff.grossInvestment,
      sumBondIssuance = ff.bondIssuance,
      sumProfitShifting = ff.profitShifting,
      sumFdiRepatriation = ff.fdiRepatriation,
      sumInventoryChange = ff.inventoryChange,
      sumCitEvasion = ff.citEvasion,
      sumEnergyCost = ff.energyCost,
      sumGreenInvestment = ff.greenInvestment,
      totalIoPaid = PLN(totalIoPaid),
      nplNew = PLN(nplNew),
      nplLoss = PLN(nplLoss),
      totalBondDefault = PLN(totalBondDefault),
      firmDeaths = firmDeaths,
      intIncome = PLN(intIncome),
      corpBondAbsorption = Ratio(corpBondAbsorption),
      actualBondIssuance = actualBondIssuance,
      netMigration = netMigration,
      perBankNewLoans = perBankNewLoans.toVector.map(PLN(_)),
      sumFirmPrincipal = ff.principalRepaid,
      perBankFirmPrincipal = perBankFirmPrincipal.toVector.map(PLN(_)),
      perBankNplDebt = perBankNplDebt.toVector,
      perBankIntIncome = perBankIntIncome.toVector,
      perBankWorkers = perBankWorkers.toVector,
      lendingRates = lendingRates,
      postFirmCrossSectorHires = postFirmCrossSectorHires,
    )
