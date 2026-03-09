package sfc.engine.steps

import sfc.agents.*
import sfc.config.{RunConfig, SimParams}
import sfc.engine.markets.{LaborMarket, SectoralMobility}
import sfc.engine.World
import sfc.types.*

import scala.util.Random

object HouseholdIncomeStep:

  case class Input(
      w: World,
      rc: RunConfig,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
  )

  case class Output(
      totalIncome: Double,
      consumption: Double,
      importCons: Double,
      domesticCons: Double,
      updatedHouseholds: Vector[Household.State],
      hhAgg: Household.Aggregates,
      perBankHhFlowsOpt: Option[PerBankHhFlows],
      pitRevenue: Double,
      importAdj: Double,
      aggUnempBenefit: Double,
  )

  def run(in: Input)(using p: SimParams): Output =
    val importAdj = p.forex.importPropensity.toDouble *
      Math.pow(p.forex.baseExRate / in.w.forex.exchangeRate, 0.5)

    val afterSep           = LaborMarket.separations(in.households, in.firms, in.firms)
    val afterWages         = LaborMarket.updateWages(afterSep, in.s2.newWage)
    val bsec               = in.w.bankingSector
    val nBanksHh           = bsec.banks.length
    val hhBankRates        = Some(
      BankRates(
        lendingRates = bsec.banks.zip(bsec.configs).map((b, cfg) => Banking.lendingRate(b, cfg, in.s1.lendingBaseRate)).toArray,
        depositRates = bsec.banks.map(_ => Banking.hhDepositRate(in.w.nbp.referenceRate.toDouble)).toArray,
      ),
    )
    val eqReturn           = in.w.equity.monthlyReturn.toDouble
    val secWages           = if p.flags.sectoralMobility then Some(SectoralMobility.sectorWages(afterWages)) else None
    val secVacancies       =
      if p.flags.sectoralMobility then Some(SectoralMobility.sectorVacancies(afterWages, in.firms)) else None
    val (newHhs, agg, pbf) = Household.step(
      afterWages,
      in.w,
      in.s2.newWage,
      in.s1.resWage,
      importAdj,
      Random,
      nBanksHh,
      hhBankRates,
      eqReturn,
      secWages,
      secVacancies,
    )

    val pitRevenue =
      if p.flags.pit then agg.totalPit.toDouble
      else 0.0

    Output(
      agg.totalIncome.toDouble,
      agg.consumption.toDouble,
      agg.importConsumption.toDouble,
      agg.domesticConsumption.toDouble,
      newHhs,
      agg,
      pbf,
      pitRevenue,
      importAdj,
      aggUnempBenefit = 0.0,
    )
