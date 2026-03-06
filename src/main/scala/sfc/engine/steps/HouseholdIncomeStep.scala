package sfc.engine.steps

import sfc.agents.*
import sfc.config.{Config, RunConfig, SECTORS}
import sfc.engine.{LaborMarket, SectoralMobility, World}
import sfc.types.*

import scala.util.Random

object HouseholdIncomeStep:

  case class Input(
    employed: Int,
    newWage: Double,
    bdp: Double,
    bdpActive: Boolean,
    resWage: Double,
    w: World,
    households: Option[Vector[Household.State]],
    firms: Array[Firm.State],
    lendingBaseRate: Double,
    newZus: SocialSecurity.ZusState,
    rc: RunConfig
  )

  case class Output(
    totalIncome: Double,
    consumption: Double,
    importCons: Double,
    domesticCons: Double,
    updatedHouseholds: Option[Vector[Household.State]],
    hhAgg: Option[Household.Aggregates],
    perBankHhFlowsOpt: Option[PerBankHhFlows],
    pitRevenue: Double,
    importAdj: Double,
    aggUnempBenefit: Double
  )

  def run(in: Input): Output =
    val importAdj = Config.ImportPropensity *
      Math.pow(Config.BaseExRate / in.w.forex.exchangeRate, 0.5)

    val aggUnempBenefit = if Config.GovUnempBenefitEnabled && in.households.isEmpty then
      val unempCount = Math.max(0, Config.TotalPopulation - in.employed)
      val avgBenefit = (Config.GovBenefitM1to3 * 3.0 + Config.GovBenefitM4to6 * 3.0) /
        Config.GovBenefitDuration.toDouble
      unempCount.toDouble * avgBenefit * Config.GovBenefitCoverage
    else 0.0

    val (totalIncome, consumption, importCons, domesticCons, updatedHouseholds, hhAgg, perBankHhFlowsOpt) =
      in.households match
        case None =>
          val avgWageMult = if Config.UnionEnabled then
            SECTORS.indices.map(i =>
              SECTORS(i).share.toDouble * (1.0 + Config.UnionWagePremium * Config.UnionDensity(i))).sum
          else 1.0
          val wageIncome = in.employed.toDouble * in.newWage * avgWageMult
          val bdpIncome  = if in.bdpActive then Config.TotalPopulation.toDouble * in.bdp else 0.0
          val grossIncome = wageIncome + bdpIncome
          val pitDeduction = if Config.PitEnabled then grossIncome * Config.PitEffectiveRate else 0.0
          val socialTransfers = if Config.Social800Enabled then
            Config.TotalPopulation.toDouble * Config.Social800ChildrenPerHh * Config.Social800Rate
          else 0.0
          val ti = grossIncome - in.newZus.contributions.toDouble + in.newZus.pensionPayments.toDouble - pitDeduction + socialTransfers + aggUnempBenefit
          val cons = ti * Config.Mpc
          val ic = cons * Math.min(0.65, importAdj)
          val dc = cons - ic
          (ti, cons, ic, dc, None, None, Option.empty[PerBankHhFlows])

        case Some(hhs) =>
          val afterSep = LaborMarket.separations(hhs, in.firms, in.firms)
          val afterWages = LaborMarket.updateWages(afterSep, in.newWage)
          val nBanksHh = in.w.bankingSector.map(_.banks.length).getOrElse(1)
          val hhBankRates = Some(BankRates(
            lendingRates = in.w.bankingSector match
              case Some(bs) => bs.banks.zip(bs.configs).map((b, cfg) =>
                Banking.lendingRate(b, cfg, in.lendingBaseRate)).toArray
              case None => Array(in.w.bank.lendingRate(in.lendingBaseRate)),
            depositRates = in.w.bankingSector match
              case Some(bs) => bs.banks.map(_ =>
                Banking.hhDepositRate(in.w.nbp.referenceRate.toDouble)).toArray
              case None => Array(Banking.hhDepositRate(in.w.nbp.referenceRate.toDouble))
          ))
          val eqReturn = in.w.equity.monthlyReturn.toDouble
          val secWages = if Config.LmSectoralMobility then Some(SectoralMobility.sectorWages(afterWages)) else None
          val secVacancies = if Config.LmSectoralMobility then Some(SectoralMobility.sectorVacancies(afterWages, in.firms)) else None
          val (newHhs, agg, pbf) = Household.step(
            afterWages, in.w, in.bdp, in.newWage, in.resWage, importAdj, Random, nBanksHh, hhBankRates, eqReturn,
            secWages, secVacancies)
          (agg.totalIncome.toDouble, agg.consumption.toDouble, agg.importConsumption.toDouble,
           agg.domesticConsumption.toDouble, Some(newHhs), Some(agg), pbf)

    val pitRevenue = if Config.PitEnabled then
      hhAgg.map(_.totalPit.toDouble).getOrElse {
        val wageIncome = in.employed.toDouble * in.newWage
        val bdpIncome  = if in.bdpActive then Config.TotalPopulation.toDouble * in.bdp else 0.0
        (wageIncome + bdpIncome) * Config.PitEffectiveRate
      }
    else 0.0

    Output(totalIncome, consumption, importCons, domesticCons, updatedHouseholds,
      hhAgg, perBankHhFlowsOpt, pitRevenue, importAdj, aggUnempBenefit)
