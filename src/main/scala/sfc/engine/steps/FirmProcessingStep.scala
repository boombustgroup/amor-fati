package sfc.engine.steps

import sfc.agents.*
import sfc.config.SimParams
import sfc.engine.markets.{CorporateBondMarket, IntermediateMarket, LaborMarket}
import sfc.engine.World
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object FirmProcessingStep:

  case class Input(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
  )

  case class Output(
      ioFirms: Vector[Firm.State],
      households: Vector[Household.State],
      sumTax: Double,
      sumCapex: Double,
      sumTechImp: Double,
      sumNewLoans: Double,
      sumEquityIssuance: Double,
      sumGrossInvestment: Double,
      sumBondIssuance: Double,
      sumProfitShifting: Double,
      sumFdiRepatriation: Double,
      sumInventoryChange: Double,
      sumCitEvasion: Double,
      sumEnergyCost: Double,
      sumGreenInvestment: Double,
      totalIoPaid: Double,
      nplNew: Double,
      nplLoss: Double,
      totalBondDefault: Double,
      firmDeaths: Int,
      intIncome: Double,
      corpBondAbsorption: Double,
      actualBondIssuance: Double,
      netMigration: Int,
      perBankNewLoans: Array[Double],
      perBankNplDebt: Array[Double],
      perBankIntIncome: Array[Double],
      perBankWorkers: Array[Int],
      lendingRates: Array[Double],
      postFirmCrossSectorHires: Int,
  )

  def run(in: Input, rng: Random)(using p: SimParams): Output =
    val bsec             = in.w.bankingSector
    val nBanks           = bsec.banks.length
    val perBankNewLoans  = new Array[Double](nBanks)
    val perBankNplDebt   = new Array[Double](nBanks)
    val perBankIntIncome = new Array[Double](nBanks)
    val perBankWorkers   = new Array[Int](nBanks)

    val currentCcyb                          = in.w.mechanisms.macropru.ccyb
    val rates                                = bsec.banks.zip(bsec.configs).map((b, cfg) => Banking.lendingRate(b, cfg, Rate(in.s1.lendingBaseRate)))
    val getFirmRate: Int => Rate             = (bankId: Int) => rates(bankId)
    val bankCanLendFn: (Int, PLN) => Boolean =
      (bankId: Int, amt: PLN) => Banking.canLend(bsec.banks(bankId), amt, rng, currentCcyb)

    val lendingRates = rates.map(_.toDouble).toArray

    var sumTax             = 0.0
    var sumCapex           = 0.0
    var sumTechImp         = 0.0
    var sumNewLoans        = 0.0
    var sumEquityIssuance  = 0.0
    var sumGrossInvestment = 0.0
    var sumBondIssuance    = 0.0
    var sumProfitShifting  = 0.0
    var sumFdiRepatriation = 0.0
    var sumInventoryChange = 0.0
    var sumCitEvasion      = 0.0
    var sumEnergyCost      = 0.0
    var sumGreenInvestment = 0.0

    val macro4firms = in.w.copy(
      month = in.s1.m,
      flows = in.w.flows.copy(sectorDemandMult = in.s4.sectorMults),
      hhAgg = in.w.hhAgg.copy(marketWage = PLN(in.s2.newWage), reservationWage = PLN(in.s1.resWage)),
    )

    val firmBondAmounts = scala.collection.mutable.HashMap.empty[FirmId, Double]

    val newFirms = in.firms.map { f =>
      val firmRate                    = getFirmRate(f.bankId.toInt)
      val firmCanLend: PLN => Boolean = amt => bankCanLendFn(f.bankId.toInt, amt)
      val r                           = Firm.process(f, macro4firms, firmRate, firmCanLend, in.firms, rng)
      sumTax += r.taxPaid.toDouble
      sumCapex += r.capexSpent.toDouble
      sumTechImp += r.techImports.toDouble
      sumGrossInvestment += r.grossInvestment.toDouble
      sumProfitShifting += r.profitShiftCost.toDouble
      sumFdiRepatriation += r.fdiRepatriation.toDouble
      sumInventoryChange += r.inventoryChange.toDouble
      sumCitEvasion += r.citEvasion.toDouble
      sumEnergyCost += r.energyCost.toDouble
      sumGreenInvestment += r.greenInvestment.toDouble

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
          (adjLoan.toDouble, eqAmt.toDouble, f2)
        else (r.newLoan.toDouble, 0.0, r.firm)

      val (finalLoan, bondAmt, bondUpdatedFirm) =
        if actualLoan > 0 && Firm.workerCount(updatedFirm) >= p.corpBond.minSize then
          val ba      = actualLoan * p.corpBond.issuanceFrac.toDouble
          val adjLoan = actualLoan - ba
          val f3      = updatedFirm.copy(
            debt = updatedFirm.debt - PLN(ba),
            bondDebt = updatedFirm.bondDebt + PLN(ba),
          )
          (adjLoan, ba, f3)
        else (actualLoan, 0.0, updatedFirm)

      sumNewLoans += finalLoan
      sumEquityIssuance += equityAmt
      sumBondIssuance += bondAmt
      if bondAmt > 0 then firmBondAmounts(f.id) = bondAmt
      perBankNewLoans(f.bankId.toInt) += finalLoan
      bondUpdatedFirm
    }

    val corpBondAbsorption =
      CorporateBondMarket
        .computeAbsorption(
          in.w.financial.corporateBonds,
          PLN(sumBondIssuance),
          in.w.bank.car,
          p.banking.minCar,
        )
        .toDouble
    val actualBondIssuance = sumBondIssuance * corpBondAbsorption
    val revertRatio        = 1.0 - corpBondAbsorption
    val adjustedFirms      =
      if revertRatio > 0.001 then
        newFirms.map { f =>
          val ba = firmBondAmounts.getOrElse(f.id, 0.0)
          if ba > 0 then
            val revert = ba * revertRatio
            f.copy(bondDebt = f.bondDebt - PLN(revert), debt = f.debt + PLN(revert))
          else f
        }
      else newFirms
    if revertRatio > 0.001 then
      val unsoldBonds = sumBondIssuance * revertRatio
      sumNewLoans += unsoldBonds
      for (fid, ba) <- firmBondAmounts do
        val revert = ba * revertRatio
        adjustedFirms.find(_.id == fid).foreach(ff => perBankNewLoans(ff.bankId.toInt) += revert)

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

    var postFirmCrossSectorHires = 0
    val afterSep                 = LaborMarket.separations(in.s3.updatedHouseholds, in.firms, ioFirms)
    val searchResult             = LaborMarket.jobSearch(afterSep, ioFirms, PLN(in.s2.newWage), rng)
    postFirmCrossSectorHires += searchResult.crossSectorHires
    val preMigrationHouseholds   = LaborMarket.updateWages(searchResult.households, PLN(in.s2.newWage))

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

    for f <- in.firms if Firm.isAlive(f) do perBankIntIncome(f.bankId.toInt) += f.debt.toDouble * rates(f.bankId.toInt).toDouble / 12.0

    val intIncome    = perBankIntIncome.kahanSum
    val netMigration = in.s2.newImmig.monthlyInflow - in.s2.newImmig.monthlyOutflow

    Output(
      ioFirms = ioFirms,
      households = finalHouseholds,
      sumTax = sumTax,
      sumCapex = sumCapex,
      sumTechImp = sumTechImp,
      sumNewLoans = sumNewLoans,
      sumEquityIssuance = sumEquityIssuance,
      sumGrossInvestment = sumGrossInvestment,
      sumBondIssuance = sumBondIssuance,
      sumProfitShifting = sumProfitShifting,
      sumFdiRepatriation = sumFdiRepatriation,
      sumInventoryChange = sumInventoryChange,
      sumCitEvasion = sumCitEvasion,
      sumEnergyCost = sumEnergyCost,
      sumGreenInvestment = sumGreenInvestment,
      totalIoPaid = totalIoPaid,
      nplNew = nplNew,
      nplLoss = nplLoss,
      totalBondDefault = totalBondDefault,
      firmDeaths = firmDeaths,
      intIncome = intIncome,
      corpBondAbsorption = corpBondAbsorption,
      actualBondIssuance = actualBondIssuance,
      netMigration = netMigration,
      perBankNewLoans = perBankNewLoans,
      perBankNplDebt = perBankNplDebt,
      perBankIntIncome = perBankIntIncome,
      perBankWorkers = perBankWorkers,
      lendingRates = lendingRates,
      postFirmCrossSectorHires = postFirmCrossSectorHires,
    )
