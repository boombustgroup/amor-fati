package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PriceEquityEconomicsSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults
  private val init        = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
  private val w           = init.world
  private val s1          = FiscalConstraintEconomics.compute(w, init.banks, ExecutionMonth.First)
  private val s2          = LaborEconomics.compute(w, init.firms, init.households, s1)
  private val s3          =
    HouseholdIncomeEconomics.compute(
      w,
      init.firms,
      init.households,
      init.banks,
      init.ledgerFinancialState,
      s1.lendingBaseRate,
      s1.resWage,
      s2.newWage,
      RandomStream.seeded(42),
    )
  private val s4          = DemandEconomics.compute(DemandEconomics.Input(w, s2.employed, s2.living, s3.domesticCons))
  private val s5          =
    FirmEconomics.runStep(w, init.firms, init.households, init.banks, init.ledgerFinancialState, s1, s2, s3, s4, RandomStream.seeded(43))

  private def runPriceStep(world: com.boombustgroup.amorfati.engine.World, firmStep: FirmEconomics.StepOutput): PriceEquityEconomics.Output =
    PriceEquityEconomics.compute(
      PriceEquityEconomics.Input(
        w = world,
        month = s1.m,
        newWage = s2.newWage,
        employed = s2.employed,
        wageGrowth = s2.wageGrowth,
        domesticCons = s3.domesticCons,
        govPurchases = s4.govPurchases,
        avgDemandMult = s4.avgDemandMult,
        sectorMults = s4.sectorMults,
        totalSystemLoans = init.banks.map(_.loans).sum,
        s5 = firmStep,
      ),
    )

  "PriceEquityEconomics.governmentDemandContribution" should "scale with constrained runtime government purchases" in {
    val low  = PriceEquityEconomics.governmentDemandContribution(PLN(250e6))
    val high = PriceEquityEconomics.governmentDemandContribution(PLN(600e6))

    high should be > low
    high.ratioTo(low).bd shouldBe (BigDecimal("2.4") +- BigDecimal("0.000000001"))
  }

  it should "respect the configured current-vs-capital multipliers" in {
    val gp           = PLN(500e6)
    val contribution = PriceEquityEconomics.governmentDemandContribution(gp)
    val expected     =
      gp * (Share.One - summon[SimParams].fiscal.govInvestShare) * summon[SimParams].fiscal.govCurrentMultiplier +
        gp * summon[SimParams].fiscal.govInvestShare * summon[SimParams].fiscal.govCapitalMultiplier

    contribution shouldBe expected
  }

  "PriceEquityEconomics.compute" should "increase direct SOE dividend extraction when the fiscal deficit is higher" in {
    val prevGdp           = w.cachedMonthlyGdpProxy.max(PLN(1.0))
    val thresholdDeficit  = prevGdp * summon[SimParams].soe.dividendFiscalThreshold
    val lowDeficitWorld   = w.copy(gov = w.gov.copy(monthly = w.gov.monthly.copy(deficit = thresholdDeficit * Multiplier(0.9))))
    val highDeficitWorld  = w.copy(gov = w.gov.copy(monthly = w.gov.monthly.copy(deficit = thresholdDeficit * Multiplier(1.1))))
    val dividendSensitive = s5.copy(
      sumRealizedPostTaxProfit = PLN(200e6),
      sumStateOwnedPostTaxProfit = PLN(100e6),
    )
    val lowDeficit        = runPriceStep(lowDeficitWorld, dividendSensitive)
    val highDeficit       = runPriceStep(highDeficitWorld, dividendSensitive)

    highDeficit.stateOwnedGovDividends should be > lowDeficit.stateOwnedGovDividends
    highDeficit.dividendTax shouldBe lowDeficit.dividendTax
    highDeficit.netDomesticDividends shouldBe lowDeficit.netDomesticDividends
  }
