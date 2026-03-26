package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
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
  private val DiasporaUnempThreshold = 0.05 // unemployment threshold for counter-cyclical remittance sensitivity

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
  def compute(
      w: World,
      month: Int,
      employed: Int,
      hhAgg: Household.Aggregates,
      @annotation.unused rng: scala.util.Random,
  )(using p: SimParams): Output =
    import ComputationBoundary.toDouble
    val hhDebtService       = hhAgg.totalDebtService
    val depositInterestPaid = hhAgg.totalDepositInterest
    val remittanceOutflow   = hhAgg.totalRemittances

    // Diaspora remittance inflow (#46)
    val diasporaInflow = if p.flags.remittance then
      val wap           = if p.flags.demographics then w.social.demographics.workingAgePop else w.totalPopulation
      val base          = toDouble(p.remittance.perCapita) * wap.toDouble
      val erAdj         = Math.pow(w.forex.exchangeRate / p.forex.baseExRate, toDouble(p.remittance.erElasticity))
      val trendAdj      = Math.pow(1.0 + toDouble(p.remittance.growthRate) / 12.0, month.toDouble)
      val unempForRemit = 1.0 - employed.toDouble / w.totalPopulation
      val cyclicalAdj   = 1.0 + toDouble(p.remittance.cyclicalSens) * Math.max(0.0, unempForRemit - DiasporaUnempThreshold)
      PLN(base * erAdj * trendAdj * cyclicalAdj)
    else PLN.Zero

    // Tourism services export/import (#47)
    val (tourismExport, tourismImport) = if p.flags.tourism then
      val monthInYear    = (month % 12) + 1
      val seasonalFactor = 1.0 + toDouble(p.tourism.seasonality) *
        Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
      val inboundErAdj   = Math.pow(w.forex.exchangeRate / p.forex.baseExRate, toDouble(p.tourism.erElasticity))
      val outboundErAdj  = Math.pow(p.forex.baseExRate / w.forex.exchangeRate, toDouble(p.tourism.erElasticity))
      val trendAdj       = Math.pow(1.0 + toDouble(p.tourism.growthRate) / 12.0, month.toDouble)
      val disruption     =
        if p.tourism.shockMonth > 0 && month >= p.tourism.shockMonth then
          toDouble(p.tourism.shockSize) * Math.pow(
            1.0 - toDouble(p.tourism.shockRecovery),
            (month - p.tourism.shockMonth).toDouble,
          )
        else 0.0
      val shockFactor    = 1.0 - disruption
      val baseGdp        = Math.max(0.0, w.gdpProxy)
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
