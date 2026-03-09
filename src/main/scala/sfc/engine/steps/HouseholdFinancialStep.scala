package sfc.engine.steps

import sfc.config.SimParams
import sfc.engine.World

object HouseholdFinancialStep:

  case class Input(
      w: World,
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
  )

  case class Output(
      hhDebtService: Double,
      depositInterestPaid: Double,
      remittanceOutflow: Double,
      diasporaInflow: Double,
      tourismExport: Double,
      tourismImport: Double,
      consumerDebtService: Double,
      consumerOrigination: Double,
      consumerDefaultAmt: Double,
      consumerNplLoss: Double,
      consumerPrincipal: Double,
  )

  def run(in: Input)(using p: SimParams): Output =
    val hhDebtService       = in.s3.hhAgg.totalDebtService.toDouble
    val depositInterestPaid = in.s3.hhAgg.totalDepositInterest.toDouble
    val remittanceOutflow   = in.s3.hhAgg.totalRemittances.toDouble

    // Diaspora remittance inflow (#46)
    val diasporaInflow = if p.flags.remittance then
      val wap           = if p.flags.demographics then in.w.demographics.workingAgePop else in.w.totalPopulation
      val base          = p.remittance.perCapita.toDouble * wap.toDouble
      val erAdj         = Math.pow(in.w.forex.exchangeRate / p.forex.baseExRate, p.remittance.erElasticity)
      val trendAdj      = Math.pow(1.0 + p.remittance.growthRate.toDouble / 12.0, in.s1.m.toDouble)
      val unempForRemit = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
      val cyclicalAdj   = 1.0 + p.remittance.cyclicalSens.toDouble * Math.max(0.0, unempForRemit - 0.05)
      base * erAdj * trendAdj * cyclicalAdj
    else 0.0

    // Tourism services export/import (#47)
    val (tourismExport, tourismImport) = if p.flags.tourism then
      val monthInYear    = (in.s1.m % 12) + 1
      val seasonalFactor = 1.0 + p.tourism.seasonality.toDouble *
        Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
      val inboundErAdj   = Math.pow(in.w.forex.exchangeRate / p.forex.baseExRate, p.tourism.erElasticity)
      val outboundErAdj  = Math.pow(p.forex.baseExRate / in.w.forex.exchangeRate, p.tourism.erElasticity)
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
