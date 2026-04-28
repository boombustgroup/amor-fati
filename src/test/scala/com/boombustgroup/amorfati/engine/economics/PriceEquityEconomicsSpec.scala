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
  private val s1          = FiscalConstraintEconomics.compute(w, init.banks, init.ledgerFinancialState, ExecutionMonth.First)
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
  private val s4          = DemandEconomics.compute(w, s2.employed, s2.living, s3.domesticCons)
  private val s5          =
    FirmEconomics.runStep(w, init.firms, init.households, init.banks, init.ledgerFinancialState, s1, s2, s3, s4, RandomStream.seeded(43))

  private def runPriceStep(world: com.boombustgroup.amorfati.engine.World, firmStep: FirmEconomics.StepOutput): PriceEquityEconomics.Output =
    PriceEquityEconomics.compute(
      w = world,
      month = s1.m,
      wageGrowth = s2.wageGrowth,
      avgDemandMult = s4.avgDemandMult,
      sectorMults = s4.sectorMults,
      totalSystemLoans = init.ledgerFinancialState.banks.map(_.firmLoan).sumPln,
      firmStep = firmStep,
    )

  "PriceEquityEconomics.governmentDemandContribution" should "scale with constrained runtime government purchases" in {
    val low  = PriceEquityEconomics.governmentDemandContribution(PLN(250000000))
    val high = PriceEquityEconomics.governmentDemandContribution(PLN(600000000))

    high should be > low
    high.ratioTo(low).bd shouldBe (BigDecimal("2.4") +- BigDecimal("0.000000001"))
  }

  it should "respect the configured current-vs-capital multipliers" in {
    val gp           = PLN(500000000)
    val contribution = PriceEquityEconomics.governmentDemandContribution(gp)
    val expected     =
      gp * (Share.One - summon[SimParams].fiscal.govInvestShare) * summon[SimParams].fiscal.govCurrentMultiplier +
        gp * summon[SimParams].fiscal.govInvestShare * summon[SimParams].fiscal.govCapitalMultiplier

    contribution shouldBe expected
  }

  "PriceEquityEconomics.compute" should "increase direct SOE dividend extraction when the fiscal deficit is higher" in {
    val prevGdp           = w.cachedMonthlyGdpProxy.max(PLN(1))
    val thresholdDeficit  = prevGdp * summon[SimParams].soe.dividendFiscalThreshold
    val lowDeficitWorld   = w.copy(gov = w.gov.copy(monthly = w.gov.monthly.copy(deficit = thresholdDeficit * Multiplier.decimal(9, 1))))
    val highDeficitWorld  = w.copy(gov = w.gov.copy(monthly = w.gov.monthly.copy(deficit = thresholdDeficit * Multiplier.decimal(11, 1))))
    val dividendSensitive = s5.copy(
      sumRealizedPostTaxProfit = PLN(200000000),
      sumStateOwnedPostTaxProfit = PLN(100000000),
    )
    val lowDeficit        = runPriceStep(lowDeficitWorld, dividendSensitive)
    val highDeficit       = runPriceStep(highDeficitWorld, dividendSensitive)

    highDeficit.stateOwnedGovDividends should be > lowDeficit.stateOwnedGovDividends
    highDeficit.dividendTax shouldBe lowDeficit.dividendTax
    highDeficit.netDomesticDividends shouldBe lowDeficit.netDomesticDividends
  }

  it should "anchor GDP proxy to realized output instead of unmet expenditure demand" in {
    val base           = runPriceStep(w, s5)
    val inflatedDemand = PriceEquityEconomics.compute(
      w = w.copy(forex = w.forex.copy(exports = w.forex.exports * 10)),
      month = s1.m,
      wageGrowth = s2.wageGrowth,
      avgDemandMult = s4.avgDemandMult,
      sectorMults = s4.sectorMults,
      totalSystemLoans = init.ledgerFinancialState.banks.map(_.firmLoan).sumPln,
      firmStep = s5,
    )

    inflatedDemand.gdp shouldBe base.gdp
  }

  it should "fail fast when sectorMults does not match the configured sector count" in {
    val err = intercept[IllegalArgumentException]:
      PriceEquityEconomics.compute(
        w = w,
        month = s1.m,
        wageGrowth = s2.wageGrowth,
        avgDemandMult = s4.avgDemandMult,
        sectorMults = s4.sectorMults.dropRight(1),
        totalSystemLoans = init.ledgerFinancialState.banks.map(_.firmLoan).sumPln,
        firmStep = s5,
      )

    err.getMessage should include("sectorMults")
    err.getMessage should include(s"${summon[SimParams].sectorDefs.length}")
    err.getMessage should include(s"${s4.sectorMults.length - 1}")
  }

  it should "fail fast when realized output aggregation sees an invalid firm sector" in {
    val invalidFirmStep = s5.copy(
      ioFirms = s5.ioFirms.updated(0, s5.ioFirms.head.copy(sector = SectorIdx(summon[SimParams].sectorDefs.length))),
    )

    val err = intercept[IllegalArgumentException]:
      runPriceStep(w, invalidFirmStep)

    err.getMessage should include("Invalid sector id")
  }

  it should "keep the first-month Poland-scale GDP proxy near the 2024 calibration baseline" in {
    val result       = runPriceStep(w, s5)
    val annualPoland = (result.gdp * 12) / summon[SimParams].gdpRatio.toMultiplier
    val baseline     = summon[SimParams].pop.realGdp
    val ratio        = decimal(annualPoland.ratioTo(baseline))

    ratio should be >= BigDecimal("0.85")
    ratio should be <= BigDecimal("1.15")
  }

  it should "keep GDP proxy close to realized sector output" in {
    val result         = runPriceStep(w, s5)
    val realizedOutput = GdpAccounting.outputBasedMonthlyGdp(result.realizedSectorOutputs, result.aggInventoryChange)

    decimal(result.gdp.ratioTo(realizedOutput)) shouldBe BigDecimal("1.0") +- BigDecimal("0.05")
  }
