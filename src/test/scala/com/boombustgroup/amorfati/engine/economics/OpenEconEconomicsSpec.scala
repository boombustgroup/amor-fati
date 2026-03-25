package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests self-contained OpenEconEconomics against old OpenEconomyStep.
  *
  * Calculus (Taylor rule, ER, trade model) must match bit-for-bit. Plumbing
  * (BoP .copy() chains) is replaced by flow mechanisms.
  */
class OpenEconEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val init = WorldInit.initialize(42L)
  private val w    = init.world
  private val rng  = new scala.util.Random(42)

  // Run old pipeline up to Step 7 to get inputs
  private val s1 = FiscalConstraintStep.run(FiscalConstraintStep.Input(w))
  private val s2 = LaborDemographicsStep.run(LaborDemographicsStep.Input(w, init.firms, init.households, s1))
  private val s3 = HouseholdIncomeStep.run(HouseholdIncomeStep.Input(w, init.firms, init.households, s1, s2), rng)
  private val s4 = DemandStep.run(DemandStep.Input(w, s2, s3))
  private val s5 = FirmProcessingStep.run(FirmProcessingStep.Input(w, init.firms, init.households, s1, s2, s3, s4), rng)
  private val s6 = HouseholdFinancialStep.run(HouseholdFinancialStep.Input(w, s1, s2, s3))
  private val s7 = PriceEquityStep.run(PriceEquityStep.Input(w, s1, s2, s3, s4, s5), rng)

  // Old result
  private val oldRng = new scala.util.Random(42)
  private val oldS8  = OpenEconomyStep.run(OpenEconomyStep.Input(w, s1, s2, s3, s4, s5, s6, s7, oldRng))

  // New result
  private val newRng    = new scala.util.Random(42)
  private val newResult = OpenEconEconomics.compute(
    OpenEconEconomics.Input(
      w = w,
      employed = s2.employed,
      newWage = s2.newWage,
      domesticConsumption = s3.domesticCons,
      importConsumption = s3.importCons,
      totalTechAndInvImports = s5.sumTechImp,
      gdp = s7.gdp,
      newInflation = s7.newInfl,
      autoRatio = s7.autoR,
      govPurchases = s4.govPurchases,
      sectorMults = s4.sectorMults,
      livingFirms = s5.ioFirms,
      totalBondDefault = s5.totalBondDefault,
      actualBondIssuance = s5.actualBondIssuance,
      corpBondAbsorption = s5.corpBondAbsorption,
      euMonthly = s7.euMonthly,
      remittanceOutflow = s6.remittanceOutflow,
      diasporaInflow = s6.diasporaInflow,
      tourismExport = s6.tourismExport,
      tourismImport = s6.tourismImport,
      equityReturn = w.financial.equity.monthlyReturn,
      investmentImports = s7.investmentImports,
      profitShifting = s5.sumProfitShifting,
      fdiRepatriation = s5.sumFdiRepatriation,
      foreignDividendOutflow = s7.foreignDividendOutflow,
      month = s1.m,
      commodityRng = newRng,
    ),
  )

  "OpenEconEconomics (self-contained)" should "match old Taylor rule rate" in {
    newResult.newRefRate shouldBe oldS8.monetary.newRefRate
  }

  it should "match old bond yield" in {
    newResult.newBondYield shouldBe oldS8.monetary.newBondYield
  }

  it should "match old interbank flows" in {
    newResult.reserveInterest shouldBe oldS8.banking.totalReserveInterest
    newResult.standingFacilityIncome shouldBe oldS8.banking.totalStandingFacilityIncome
    newResult.interbankInterest shouldBe oldS8.banking.totalInterbankInterest
  }

  it should "match old bank bond income" in {
    newResult.bankBondIncome shouldBe oldS8.banking.bankBondIncome
  }

  it should "produce flows that close at SFC == 0L" in {
    val flows = OpenEconFlows.emit(
      OpenEconFlows.Input(
        newResult.exports,
        newResult.totalImports,
        s6.tourismExport,
        s6.tourismImport,
        newResult.fdi,
        newResult.portfolioFlows,
        newResult.primaryIncome,
        newResult.euFunds,
        s6.diasporaInflow,
        PLN.Zero,
      ),
    )
    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
  }
