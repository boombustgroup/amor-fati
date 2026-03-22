package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.types.*

/** Household financial flows: mortgage debt service, deposit interest, diaspora
  * remittance inflows (NBP BoP 2024), tourism services (GUS TSA 2023), and
  * consumer credit aggregation. Connects household-level flows to the banking
  * sector and external accounts.
  */
object HouseholdFinancialStep:

  // ---- Calibration constants ----
  private val DiasporaUnempThreshold = 0.05 // unemployment threshold for counter-cyclical remittance sensitivity

  case class Input(
      w: World,                         // current world state
      s1: FiscalConstraintStep.Output,  // fiscal constraint (month counter)
      s2: LaborDemographicsStep.Output, // labor/demographics (employment)
      s3: HouseholdIncomeStep.Output,   // household income (aggregates)
  )

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

  def run(in: Input)(using p: SimParams): Output =
    val hhDebtService       = in.s3.hhAgg.totalDebtService.toDouble
    val depositInterestPaid = in.s3.hhAgg.totalDepositInterest.toDouble
    val remittanceOutflow   = in.s3.hhAgg.totalRemittances.toDouble

    // Diaspora remittance inflow (#46)
    val diasporaInflow = if p.flags.remittance then
      val wap           = if p.flags.demographics then in.w.social.demographics.workingAgePop else in.w.totalPopulation
      val base          = p.remittance.perCapita.toDouble * wap.toDouble
      val erAdj         = Math.pow(in.w.forex.exchangeRate / p.forex.baseExRate, p.remittance.erElasticity.toDouble)
      val trendAdj      = Math.pow(1.0 + p.remittance.growthRate.toDouble / 12.0, in.s1.m.toDouble)
      val unempForRemit = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
      val cyclicalAdj   = 1.0 + p.remittance.cyclicalSens.toDouble * Math.max(0.0, unempForRemit - DiasporaUnempThreshold)
      base * erAdj * trendAdj * cyclicalAdj
    else 0.0

    // Tourism services export/import (#47)
    val (tourismExport, tourismImport) = if p.flags.tourism then
      val monthInYear    = (in.s1.m % 12) + 1
      val seasonalFactor = 1.0 + p.tourism.seasonality.toDouble *
        Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
      val inboundErAdj   = Math.pow(in.w.forex.exchangeRate / p.forex.baseExRate, p.tourism.erElasticity.toDouble)
      val outboundErAdj  = Math.pow(p.forex.baseExRate / in.w.forex.exchangeRate, p.tourism.erElasticity.toDouble)
      val trendAdj       = Math.pow(1.0 + p.tourism.growthRate.toDouble / 12.0, in.s1.m.toDouble)
      val disruption     =
        if p.tourism.shockMonth > 0 && in.s1.m >= p.tourism.shockMonth then
          p.tourism.shockSize.toDouble * Math.pow(
            1.0 - p.tourism.shockRecovery.toDouble,
            (in.s1.m - p.tourism.shockMonth).toDouble,
          )
        else 0.0
      val shockFactor    = 1.0 - disruption
      val baseGdp        = Math.max(0.0, in.w.gdpProxy)
      val inbound        = Math.max(
        0.0,
        baseGdp * p.tourism.inboundShare.toDouble *
          seasonalFactor * inboundErAdj * trendAdj * shockFactor,
      )
      val outbound       = Math.max(
        0.0,
        baseGdp * p.tourism.outboundShare.toDouble *
          seasonalFactor * outboundErAdj * trendAdj * shockFactor,
      )
      (inbound, outbound)
    else (0.0, 0.0)

    // Consumer credit flows
    val consumerDebtService = in.s3.hhAgg.totalConsumerDebtService.toDouble
    val consumerOrigination = in.s3.hhAgg.totalConsumerOrigination.toDouble
    val consumerDefaultAmt  = in.s3.hhAgg.totalConsumerDefault.toDouble
    val consumerNplLoss     = consumerDefaultAmt * (1.0 - p.household.ccNplRecovery.toDouble)
    val consumerPrincipal   = in.s3.hhAgg.totalConsumerPrincipal.toDouble

    Output(
      PLN(hhDebtService),
      PLN(depositInterestPaid),
      PLN(remittanceOutflow),
      PLN(diasporaInflow),
      PLN(tourismExport),
      PLN(tourismImport),
      PLN(consumerDebtService),
      PLN(consumerOrigination),
      PLN(consumerDefaultAmt),
      PLN(consumerNplLoss),
      PLN(consumerPrincipal),
    )
