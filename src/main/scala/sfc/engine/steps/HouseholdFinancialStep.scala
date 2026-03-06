package sfc.engine.steps

import sfc.agents.*
import sfc.config.Config
import sfc.types.*

object HouseholdFinancialStep:

  case class Input(
    hhAgg: Option[Household.Aggregates],
    newImmigRemittanceOutflow: Double,
    employed: Int,
    m: Int,
    lendingBaseRate: Double,
    domesticCons: Double,
    forexExchangeRate: Double,
    gdpProxy: Double,
    demographicsWorkingAgePop: Int,
    bankConsumerLoans: Double,
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
    val hhDebtService = in.hhAgg.map(_.totalDebtService.toDouble).getOrElse(0.0)
    val depositInterestPaid = in.hhAgg.map(_.totalDepositInterest.toDouble).getOrElse(0.0)
    val remittanceOutflow = in.hhAgg.map(_.totalRemittances.toDouble).getOrElse(in.newImmigRemittanceOutflow)

    // Diaspora remittance inflow (#46)
    val diasporaInflow = if Config.RemittanceEnabled then
      val wap = if Config.DemEnabled then in.demographicsWorkingAgePop else Config.TotalPopulation
      val base = Config.RemittancePerCapita * wap.toDouble
      val erAdj = Math.pow(in.forexExchangeRate / Config.BaseExRate, Config.RemittanceErElasticity)
      val trendAdj = Math.pow(1.0 + Config.RemittanceGrowthRate / 12.0, in.m.toDouble)
      val unempForRemit = 1.0 - in.employed.toDouble / Config.TotalPopulation
      val cyclicalAdj = 1.0 + Config.RemittanceCyclicalSens * Math.max(0.0, unempForRemit - 0.05)
      base * erAdj * trendAdj * cyclicalAdj
    else 0.0

    // Tourism services export/import (#47)
    val (tourismExport, tourismImport) = if Config.TourismEnabled then
      val monthInYear = (in.m % 12) + 1
      val seasonalFactor = 1.0 + Config.TourismSeasonality *
        Math.cos(2 * Math.PI * (monthInYear - Config.TourismPeakMonth) / 12.0)
      val inboundErAdj = Math.pow(in.forexExchangeRate / Config.BaseExRate, Config.TourismErElasticity)
      val outboundErAdj = Math.pow(Config.BaseExRate / in.forexExchangeRate, Config.TourismErElasticity)
      val trendAdj = Math.pow(1.0 + Config.TourismGrowthRate / 12.0, in.m.toDouble)
      val disruption =
        if Config.TourismShockMonth > 0 && in.m >= Config.TourismShockMonth then
          Config.TourismShockSize * Math.pow(
            1.0 - Config.TourismShockRecovery,
            (in.m - Config.TourismShockMonth).toDouble,
          )
        else 0.0
      val shockFactor = 1.0 - disruption
      val baseGdp = Math.max(0.0, in.gdpProxy)
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
    val aggConsumerDS = in.bankConsumerLoans * (Config.CcAmortRate + (in.lendingBaseRate + Config.CcSpread) / 12.0)
    val aggConsumerOrig = in.domesticCons * 0.02
    val consumerDebtService = in.hhAgg.map(_.totalConsumerDebtService.toDouble).getOrElse(aggConsumerDS)
    val consumerOrigination = in.hhAgg.map(_.totalConsumerOrigination.toDouble).getOrElse(aggConsumerOrig)
    val consumerDefaultAmt = in.hhAgg.map(_.totalConsumerDefault.toDouble).getOrElse(0.0)
    val consumerNplLoss = consumerDefaultAmt * (1.0 - Config.CcNplRecovery)
    val consumerPrincipal = in.hhAgg.map(_.totalConsumerPrincipal.toDouble).getOrElse {
      if Config.CcAmortRate + (in.lendingBaseRate + Config.CcSpread) / 12.0 > 0 then
        consumerDebtService * (Config.CcAmortRate / (Config.CcAmortRate + (in.lendingBaseRate + Config.CcSpread) / 12.0))
      else 0.0
    }

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
