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

  @boundaryEscape
  def run(in: Input)(using p: SimParams): Output =
    import ComputationBoundary.toDouble
    val hhDebtService       = in.s3.hhAgg.totalDebtService
    val depositInterestPaid = in.s3.hhAgg.totalDepositInterest
    val remittanceOutflow   = in.s3.hhAgg.totalRemittances

    // Diaspora remittance inflow (#46)
    val diasporaInflow = if p.flags.remittance then
      val wap           = if p.flags.demographics then in.w.social.demographics.workingAgePop else in.w.totalPopulation
      val base          = toDouble(p.remittance.perCapita) * wap.toDouble
      val erAdj         = Math.pow(in.w.forex.exchangeRate / p.forex.baseExRate, toDouble(p.remittance.erElasticity))
      val trendAdj      = Math.pow(1.0 + toDouble(p.remittance.growthRate) / 12.0, in.s1.m.toDouble)
      val unempForRemit = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
      val cyclicalAdj   = 1.0 + toDouble(p.remittance.cyclicalSens) * Math.max(0.0, unempForRemit - DiasporaUnempThreshold)
      PLN(base * erAdj * trendAdj * cyclicalAdj)
    else PLN.Zero

    // Tourism services export/import (#47)
    val (tourismExport, tourismImport) = if p.flags.tourism then
      val monthInYear    = (in.s1.m % 12) + 1
      val seasonalFactor = 1.0 + toDouble(p.tourism.seasonality) *
        Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
      val inboundErAdj   = Math.pow(in.w.forex.exchangeRate / p.forex.baseExRate, toDouble(p.tourism.erElasticity))
      val outboundErAdj  = Math.pow(p.forex.baseExRate / in.w.forex.exchangeRate, toDouble(p.tourism.erElasticity))
      val trendAdj       = Math.pow(1.0 + toDouble(p.tourism.growthRate) / 12.0, in.s1.m.toDouble)
      val disruption     =
        if p.tourism.shockMonth > 0 && in.s1.m >= p.tourism.shockMonth then
          toDouble(p.tourism.shockSize) * Math.pow(
            1.0 - toDouble(p.tourism.shockRecovery),
            (in.s1.m - p.tourism.shockMonth).toDouble,
          )
        else 0.0
      val shockFactor    = 1.0 - disruption
      val baseGdp        = Math.max(0.0, in.w.gdpProxy)
      val inbound        = Math.max(
        0.0,
        baseGdp * toDouble(p.tourism.inboundShare) *
          seasonalFactor * inboundErAdj * trendAdj * shockFactor,
      )
      val outbound       = Math.max(
        0.0,
        baseGdp * toDouble(p.tourism.outboundShare) *
          seasonalFactor * outboundErAdj * trendAdj * shockFactor,
      )
      (PLN(inbound), PLN(outbound))
    else (PLN.Zero, PLN.Zero)

    // Consumer credit flows
    val consumerDebtService = in.s3.hhAgg.totalConsumerDebtService
    val consumerOrigination = in.s3.hhAgg.totalConsumerOrigination
    val consumerDefaultAmt  = in.s3.hhAgg.totalConsumerDefault
    val consumerNplLoss     = consumerDefaultAmt * (Share.One - p.household.ccNplRecovery)
    val consumerPrincipal   = in.s3.hhAgg.totalConsumerPrincipal

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
