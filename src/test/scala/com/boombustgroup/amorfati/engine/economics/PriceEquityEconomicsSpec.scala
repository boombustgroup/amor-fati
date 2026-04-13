package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.agents.{EarmarkedFunds, SocialSecurity}
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
  private val fiscal      = FiscalConstraintEconomics.compute(w, init.banks, ExecutionMonth.First)
  private val s1          = FiscalConstraintEconomics.toOutput(fiscal)
  private val labor       = LaborEconomics.compute(w, init.firms, init.households, s1)
  private val s2          = LaborEconomics.Output(
    labor.wage,
    labor.employed,
    labor.laborDemand,
    labor.wageGrowth,
    labor.operationalHiringSlack,
    labor.immigration,
    labor.netMigration,
    labor.demographics,
    SocialSecurity.ZusState.zero,
    SocialSecurity.NfzState.zero,
    SocialSecurity.PpkState.zero,
    PLN.Zero,
    EarmarkedFunds.State.zero,
    labor.living,
    labor.regionalWages,
  )
  private val s3          =
    HouseholdIncomeEconomics.compute(w, init.firms, init.households, init.banks, s1.lendingBaseRate, s1.resWage, s2.newWage, RandomStream.seeded(42))
  private val s4          = DemandEconomics.compute(DemandEconomics.Input(w, s2.employed, s2.living, s3.domesticCons))
  private val s5          = FirmEconomics.runStep(w, init.firms, init.households, init.banks, s1, s2, s3, s4, RandomStream.seeded(43))

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
        banks = init.banks,
        s5 = firmStep,
      ),
      RandomStream.seeded(44),
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
    val lowDeficitWorld   = w.copy(gov = w.gov.copy(monthly = w.gov.monthly.copy(deficit = prevGdp * Share(0.01))))
    val highDeficitWorld  = w.copy(gov = w.gov.copy(monthly = w.gov.monthly.copy(deficit = prevGdp * Share(0.10))))
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
