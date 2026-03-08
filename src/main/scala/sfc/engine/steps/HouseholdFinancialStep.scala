package sfc.engine.steps

import sfc.config.Config
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

  def run(in: Input): Output =
    val hhDebtService = in.s3.hhAgg.totalDebtService.toDouble
    val depositInterestPaid = in.s3.hhAgg.totalDepositInterest.toDouble
    val remittanceOutflow = in.s3.hhAgg.totalRemittances.toDouble

    // Diaspora remittance inflow (#46)
    val diasporaInflow = if Config.RemittanceEnabled then
      val wap = if Config.DemEnabled then in.w.demographics.workingAgePop else Config.TotalPopulation
      val base = Config.RemittancePerCapita * wap.toDouble
      val erAdj = Math.pow(in.w.forex.exchangeRate / Config.BaseExRate, Config.RemittanceErElasticity)
      val trendAdj = Math.pow(1.0 + Config.RemittanceGrowthRate / 12.0, in.s1.m.toDouble)
      val unempForRemit = 1.0 - in.s2.employed.toDouble / Config.TotalPopulation
      val cyclicalAdj = 1.0 + Config.RemittanceCyclicalSens * Math.max(0.0, unempForRemit - 0.05)
      base * erAdj * trendAdj * cyclicalAdj
    else 0.0

    // Tourism services export/import (#47)
    val (tourismExport, tourismImport) = if Config.TourismEnabled then
      val monthInYear = (in.s1.m % 12) + 1
      val seasonalFactor = 1.0 + Config.TourismSeasonality *
        Math.cos(2 * Math.PI * (monthInYear - Config.TourismPeakMonth) / 12.0)
      val inboundErAdj = Math.pow(in.w.forex.exchangeRate / Config.BaseExRate, Config.TourismErElasticity)
      val outboundErAdj = Math.pow(Config.BaseExRate / in.w.forex.exchangeRate, Config.TourismErElasticity)
      val trendAdj = Math.pow(1.0 + Config.TourismGrowthRate / 12.0, in.s1.m.toDouble)
      val disruption =
        if Config.TourismShockMonth > 0 && in.s1.m >= Config.TourismShockMonth then
          Config.TourismShockSize * Math.pow(
            1.0 - Config.TourismShockRecovery,
            (in.s1.m - Config.TourismShockMonth).toDouble,
          )
        else 0.0
      val shockFactor = 1.0 - disruption
      val baseGdp = Math.max(0.0, in.w.gdpProxy)
      val inbound = Math.max(
        0.0,
        baseGdp * Config.TourismInboundShare *
          seasonalFactor * inboundErAdj * trendAdj * shockFactor,
      )
      val outbound = Math.max(
        0.0,
        baseGdp * Config.TourismOutboundShare *
          seasonalFactor * outboundErAdj * trendAdj * shockFactor,
      )
      (inbound, outbound)
    else (0.0, 0.0)

    // Consumer credit flows
    val consumerDebtService = in.s3.hhAgg.totalConsumerDebtService.toDouble
    val consumerOrigination = in.s3.hhAgg.totalConsumerOrigination.toDouble
    val consumerDefaultAmt = in.s3.hhAgg.totalConsumerDefault.toDouble
    val consumerNplLoss = consumerDefaultAmt * (1.0 - Config.CcNplRecovery)
    val consumerPrincipal = in.s3.hhAgg.totalConsumerPrincipal.toDouble

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
