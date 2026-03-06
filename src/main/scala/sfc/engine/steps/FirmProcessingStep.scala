package sfc.engine.steps

import sfc.agents.*
import sfc.config.{Config, RunConfig, SECTORS}
import sfc.engine.{CorporateBondMarket, IntermediateMarket, LaborMarket, World}
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object FirmProcessingStep:

  case class Input(
    w: World,
    firms: Array[Firm.State],
    households: Option[Vector[Household.State]],
    updatedHouseholds: Option[Vector[Household.State]],
    newWage: Double,
    resWage: Double,
    m: Int,
    sectorMults: Vector[Double],
    lendingBaseRate: Double,
    newImmig: Immigration.State,
    rc: RunConfig,
  )

  case class Output(
    ioFirms: Array[Firm.State],
    finalHouseholds: Option[Vector[Household.State]],
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

  def run(in: Input): Output =
    val nBanks = in.w.bankingSector.map(_.banks.length).getOrElse(1)
    val perBankNewLoans = new Array[Double](nBanks)
    val perBankNplDebt = new Array[Double](nBanks)
    val perBankIntIncome = new Array[Double](nBanks)
    val perBankWorkers = new Array[Int](nBanks)

    val currentCcyb = in.w.macropru.ccyb.toDouble
    val (getLendRate, bankCanLendFn): (Int => Double, (Int, Double) => Boolean) =
      in.w.bankingSector match
        case Some(bs) =>
          val rates = bs.banks.zip(bs.configs).map((b, cfg) => Banking.lendingRate(b, cfg, in.lendingBaseRate))
          (
            (bankId: Int) => rates(bankId),
            (bankId: Int, amt: Double) => Banking.canLend(bs.banks(bankId), amt, Random, currentCcyb),
          )
        case None =>
          val rate = in.w.bank.lendingRate(in.lendingBaseRate)
          val canLendFn: (Int, Double) => Boolean = (_: Int, amt: Double) => {
            val approvalP = Math.max(0.1, 1.0 - in.w.bank.nplRatio * 3.0)
            in.w.bank.canLend(amt) && Random.nextDouble() < approvalP
          }
          ((_: Int) => rate, canLendFn)

    val lendingRates = (0 until nBanks).map(getLendRate).toArray

    var sumTax = 0.0
    var sumCapex = 0.0
    var sumTechImp = 0.0
    var sumNewLoans = 0.0
    var sumEquityIssuance = 0.0
    var sumGrossInvestment = 0.0
    var sumBondIssuance = 0.0
    var sumProfitShifting = 0.0
    var sumFdiRepatriation = 0.0
    var sumInventoryChange = 0.0
    var sumCitEvasion = 0.0
    var sumEnergyCost = 0.0
    var sumGreenInvestment = 0.0

    val macro4firms = in.w.copy(
      month = in.m,
      sectorDemandMult = in.sectorMults,
      hh = in.w.hh.copy(marketWage = PLN(in.newWage), reservationWage = PLN(in.resWage)),
    )

    val firmBondAmounts = scala.collection.mutable.HashMap.empty[FirmId, Double]

    val newFirms = in.firms.map { f =>
      val firmRate = getLendRate(f.bankId.toInt)
      val firmCanLend: Double => Boolean = amt => bankCanLendFn(f.bankId.toInt, amt)
      val r = Firm.process(f, macro4firms, firmRate, firmCanLend, in.firms, in.rc)
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
        if Config.GpwEnabled && Config.GpwEquityIssuance && r.newLoan > PLN.Zero &&
          Firm.workers(r.firm) >= Config.GpwIssuanceMinSize
        then
          val eqAmt = r.newLoan * Config.GpwIssuanceFrac
          val adjLoan = r.newLoan - eqAmt
          val f2 = r.firm.copy(
            debt = r.firm.debt - eqAmt,
            equityRaised = r.firm.equityRaised + eqAmt,
          )
          (adjLoan.toDouble, eqAmt.toDouble, f2)
        else (r.newLoan.toDouble, 0.0, r.firm)

      val (finalLoan, bondAmt, bondUpdatedFirm) =
        if actualLoan > 0 && Firm.workers(updatedFirm) >= Config.CorpBondMinSize then
          val ba = actualLoan * Config.CorpBondIssuanceFrac
          val adjLoan = actualLoan - ba
          val f3 = updatedFirm.copy(
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
      CorporateBondMarket.computeAbsorption(in.w.corporateBonds, sumBondIssuance, in.w.bank.car, Config.MinCar)
    val actualBondIssuance = sumBondIssuance * corpBondAbsorption
    val revertRatio = 1.0 - corpBondAbsorption
    val adjustedFirms =
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

    for f <- adjustedFirms if Firm.isAlive(f) do perBankWorkers(f.bankId.toInt) += Firm.workers(f)

    val (ioFirms, totalIoPaid) = if Config.IoEnabled then
      val r = IntermediateMarket.process(
        adjustedFirms,
        in.sectorMults,
        in.w.priceLevel,
        Config.IoMatrix,
        Config.IoColumnSums,
        Config.IoScale,
      )
      (r.firms, r.totalPaid)
    else (adjustedFirms, 0.0)

    var postFirmCrossSectorHires = 0
    val preMigrationHouseholds = in.updatedHouseholds.map { hhs =>
      val afterSep = LaborMarket.separations(hhs, in.firms, ioFirms)
      val (afterSearch, csHires) = LaborMarket.jobSearch(afterSep, ioFirms, in.newWage, Random)
      postFirmCrossSectorHires += csHires
      LaborMarket.updateWages(afterSearch, in.newWage)
    }

    val finalHouseholds =
      if Config.ImmigEnabled then
        preMigrationHouseholds.map { hhs =>
          val afterRemoval = Immigration.removeReturnMigrants(hhs, in.newImmig.monthlyOutflow)
          val startId = afterRemoval.map(_.id).maxOption.getOrElse(-1) + 1
          val newImmigrants = Immigration.spawnImmigrants(in.newImmig.monthlyInflow, startId, Random)
          afterRemoval ++ newImmigrants
        }
      else preMigrationHouseholds

    val prevAlive = in.firms.filter(Firm.isAlive).map(_.id).toSet
    val newlyDead = ioFirms.filter(f => !Firm.isAlive(f) && prevAlive.contains(f.id))
    val firmDeaths = newlyDead.length
    val nplNew = newlyDead.kahanSumBy(_.debt.toDouble)
    val nplLoss = nplNew * (1.0 - Config.LoanRecovery)
    val totalBondDefault = newlyDead.kahanSumBy(_.bondDebt.toDouble)

    for f <- newlyDead do perBankNplDebt(f.bankId.toInt) += f.debt.toDouble

    for f <- in.firms if Firm.isAlive(f) do
      perBankIntIncome(f.bankId.toInt) += f.debt.toDouble * getLendRate(f.bankId.toInt) / 12.0

    val intIncome = perBankIntIncome.kahanSum
    val netMigration = in.newImmig.monthlyInflow - in.newImmig.monthlyOutflow

    Output(
      ioFirms = ioFirms,
      finalHouseholds = finalHouseholds,
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
