package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*

/** Pure economic logic for household financial flows — no state mutation, no
  * flows.
  *
  * Computes mortgage debt service, deposit interest, diaspora remittance
  * inflows (NBP BoP 2024), tourism services (GUS TSA 2023), and consumer credit
  * aggregation. Connects household-level flows to the banking sector and
  * external accounts.
  *
  * Extracted from HouseholdFinancialStep (Calculus vs Accounting split).
  */
object HouseholdFinancialEconomics:

  // ---- Calibration constants ----
  private val DiasporaUnempThreshold = Share.decimal(5, 2) // unemployment threshold for counter-cyclical remittance sensitivity

  private def trendFactor(annualGrowth: Rate, elapsedMonths: Int): Multiplier =
    annualGrowth.monthly.growthMultiplier.pow(Scalar(elapsedMonths))

  private def monthlySeasonalCos(monthInYear: Int, peakMonth: Int): Coefficient =
    Math.floorMod(monthInYear - peakMonth, 12) match
      case 0  => Coefficient.One
      case 1  => Coefficient.decimal(8660254038L, 10)
      case 2  => Coefficient.decimal(5, 1)
      case 3  => Coefficient.Zero
      case 4  => Coefficient.decimal(-5, 1)
      case 5  => Coefficient.decimal(-8660254038L, 10)
      case 6  => Coefficient(-1)
      case 7  => Coefficient.decimal(-8660254038L, 10)
      case 8  => Coefficient.decimal(-5, 1)
      case 9  => Coefficient.Zero
      case 10 => Coefficient.decimal(5, 1)
      case _  => Coefficient.decimal(8660254038L, 10)

  case class Output(
      hhDebtService: PLN,       // total household mortgage debt service
      depositInterestPaid: PLN, // total deposit interest paid to households
      remittanceOutflow: PLN,   // total household remittance outflow
      diasporaInflow: PLN,      // diaspora remittance inflow (NBP BoP)
      tourismExport: PLN,       // inbound tourism receipts
      tourismImport: PLN,       // outbound tourism expenditure
      consumerDebtService: PLN, // total consumer credit debt service
      consumerOrigination: PLN, // new consumer loans originated
      consumerDefaultAmt: PLN,  // consumer loan default amount
      consumerNplLoss: PLN,     // consumer NPL loss net of recovery
      consumerPrincipal: PLN,   // consumer loan principal repayment
  )

  def compute(
      w: World,
      month: ExecutionMonth,
      employed: Int,
      hhAgg: Household.Aggregates,
      @annotation.unused rng: RandomStream,
  )(using p: SimParams): Output =
    val hhDebtService       = hhAgg.totalDebtService
    val depositInterestPaid = hhAgg.totalDepositInterest
    val remittanceOutflow   = hhAgg.totalRemittances

    // Diaspora remittance inflow (#46)
    val diasporaInflow =
      val wap           = w.social.demographics.workingAgePop
      val elapsedMonths = month.previousCompleted.toInt
      val base          = p.remittance.perCapita * wap
      val erAdj         = w.forex.exchangeRate.ratioTo(p.forex.baseExRate).pow(p.remittance.erElasticity.toScalar)
      val trendAdj      = trendFactor(p.remittance.growthRate, elapsedMonths)
      val unempGap      = (w.unemploymentRate(employed) - DiasporaUnempThreshold).max(Share.Zero)
      val cyclicalAdj   = (p.remittance.cyclicalSens * unempGap).growthMultiplier
      base * erAdj * trendAdj * cyclicalAdj

    // Tourism services export/import (#47)
    val (tourismExport, tourismImport) =
      val monthInt       = month.toInt
      val elapsedMonths  = month.previousCompleted.toInt
      val monthInYear    = month.monthInYear
      val seasonalFactor = (p.tourism.seasonality * monthlySeasonalCos(monthInYear, p.tourism.peakMonth)).growthMultiplier
      val inboundErAdj   = w.forex.exchangeRate.ratioTo(p.forex.baseExRate).pow(p.tourism.erElasticity.toScalar)
      val outboundErAdj  = p.forex.baseExRate.ratioTo(w.forex.exchangeRate).pow(p.tourism.erElasticity.toScalar)
      val trendAdj       = trendFactor(p.tourism.growthRate, elapsedMonths)
      val shockFactor    =
        if p.tourism.shockMonth > 0 && monthInt >= p.tourism.shockMonth then
          val decay = (Rate.Zero - p.tourism.shockRecovery).growthMultiplier.pow(Scalar(monthInt - p.tourism.shockMonth))
          Multiplier.One - (p.tourism.shockSize * decay)
        else Multiplier.One
      val baseGdp        = w.cachedMonthlyGdpProxy.max(PLN.Zero)
      val inbound        = baseGdp * p.tourism.inboundShare * seasonalFactor * inboundErAdj * trendAdj * shockFactor
      val outbound       = baseGdp * p.tourism.outboundShare * seasonalFactor * outboundErAdj * trendAdj * shockFactor
      (inbound.max(PLN.Zero), outbound.max(PLN.Zero))

    // Consumer credit flows
    val consumerDebtService = hhAgg.totalConsumerDebtService
    val consumerOrigination = hhAgg.totalConsumerOrigination
    val consumerDefaultAmt  = hhAgg.totalConsumerDefault
    val consumerNplLoss     = consumerDefaultAmt * (Share.One - p.household.ccNplRecovery)
    val consumerPrincipal   = hhAgg.totalConsumerPrincipal

    Output(
      hhDebtService,
      depositInterestPaid,
      remittanceOutflow,
      diasporaInflow,
      tourismExport,
      tourismImport,
      consumerDebtService,
      consumerOrigination,
      consumerDefaultAmt,
      consumerNplLoss,
      consumerPrincipal,
    )
