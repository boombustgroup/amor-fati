package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.{EquityMarket, PriceLevel}
import com.boombustgroup.amorfati.engine.mechanisms.{EuFunds, Macroprudential}
import com.boombustgroup.amorfati.types.*

/** Pure economic logic for price level dynamics, GPW equity market, GDP
  * computation, macroprudential policy, and Arthur-style sigma evolution — no
  * state mutation, no flows.
  *
  * Integrates real-side aggregates with financial-market state.
  *
  * Extracted from PriceEquityStep (Calculus vs Accounting split).
  */
object PriceEquityEconomics:

  case class Output(
      autoR: Share,
      hybR: Share,
      aggInventoryStock: PLN,
      aggGreenCapital: PLN,
      euMonthly: PLN,
      euCofin: PLN,
      euProjectCapital: PLN,
      gdp: PLN,
      realizedSectorOutputs: Vector[PLN],
      newMacropru: Macroprudential.State,
      newSigmas: Vector[Sigma],
      newInfl: Rate,
      newPrice: PriceIndex,
      equityAfterIssuance: EquityMarket.State,
      netDomesticDividends: PLN,
      foreignDividendOutflow: PLN,
      dividendTax: PLN,
      stateOwnedGovDividends: PLN,
      firmProfits: PLN,
      domesticGFCF: PLN,
      investmentImports: PLN,
      aggInventoryChange: PLN,
  )

  // ---------------------------------------------------------------------------
  // Sigma dynamics — Arthur-style increasing returns / learning-by-doing
  // ---------------------------------------------------------------------------
  //
  // Per-sector technological sophistication (sigma) evolves endogenously as firms
  // adopt automation. The mechanism captures a key insight from W. Brian Arthur's
  // increasing returns theory: technologies that get adopted become *better* (or
  // at least more productive), reinforcing further adoption.
  //
  // The update rule is a logistic growth equation:
  //
  //   sigma_s(t+1) = sigma_s(t) + lambda * sigma_s(t) * adoptionRate_s(t) * (1 - sigma_s(t) / cap)
  //
  // where:
  //   - lambda       = learning rate (p.firm.sigmaLambda); 0.0 disables the mechanism entirely
  //   - adoptionRate = fraction of alive firms in sector s that are Automated or Hybrid
  //   - cap          = baseSigma * capMult — the logistic ceiling; sigma saturates at cap
  //
  // Two safety constraints:
  //   - **Ratchet**: sigma never decreases (knowledge is sticky — once learned, not forgotten)
  //   - **Hard cap**: sigma never exceeds baseSigma * capMult (bounded rationality / diminishing returns)
  //
  // The ratchet + logistic combination produces an S-curve: slow initial adoption in each sector,
  // rapid mid-phase growth as network externalities kick in, then saturation as the sector approaches
  // its technological frontier.
  //
  // Reference: Arthur, W.B. (1989), "Competing Technologies, Increasing Returns, and Lock-In by
  // Historical Events", The Economic Journal 99(394).
  // ---------------------------------------------------------------------------

  /** Evolve per-sector sigma via Arthur-style increasing returns /
    * learning-by-doing.
    *
    * @param currentSigmas
    *   current per-sector sigma values
    * @param baseSigmas
    *   initial per-sector sigma (used to compute logistic cap)
    * @param sectorAdoption
    *   per-sector adoption rates (fraction automated+hybrid)
    * @param lambda
    *   learning rate (0.0 = static mode, no-op)
    * @param capMult
    *   logistic ceiling multiplier (cap = baseSigma * capMult)
    * @return
    *   updated sigma vector (never decreasing — ratchet)
    */
  private[economics] def evolveSigmas(
      currentSigmas: Vector[Sigma],
      baseSigmas: Vector[Sigma],
      sectorAdoption: Vector[Share],
      lambda: Coefficient,
      capMult: Multiplier,
  ): Vector[Sigma] =
    if lambda == Coefficient.Zero then currentSigmas
    else
      currentSigmas.zip(baseSigmas).zip(sectorAdoption).map { case ((sigma, base), adoption) =>
        val sigmaLevel    = sigma.toCoefficient
        val cap           = Sigma.fromRaw((base.toScalar * capMult).toLong)
        val saturationGap = (Coefficient.One - sigma.toScalar.ratioTo(cap.toScalar).toCoefficient).max(Coefficient.Zero)
        val delta         = sigma.toCoefficient * lambda.max(Coefficient.Zero) * adoption.toCoefficient * saturationGap
        val upperBound    = cap.toCoefficient.max(sigmaLevel)
        val candidate     = (sigmaLevel + delta).max(sigmaLevel).min(upperBound)
        Sigma.fromRaw(candidate.toLong)
      }

  private[economics] def governmentDemandContribution(govPurchases: PLN)(using p: SimParams): PLN =
    govPurchases * (Share.One - p.fiscal.govInvestShare) * p.fiscal.govCurrentMultiplier +
      govPurchases * p.fiscal.govInvestShare * p.fiscal.govCapitalMultiplier

  private[economics] def fiscalDeficitToGdp(deficit: PLN, monthlyGdp: PLN): Share =
    if deficit > PLN.Zero && monthlyGdp > PLN.Zero then deficit.ratioTo(monthlyGdp).toShare.clamp(Share.Zero, Share.One)
    else Share.Zero

  // ---------------------------------------------------------------------------
  // Main compute logic
  // ---------------------------------------------------------------------------

  def compute(
      w: World,
      month: ExecutionMonth,
      wageGrowth: Coefficient,
      avgDemandMult: Multiplier,
      sectorMults: Vector[Multiplier],
      totalSystemLoans: PLN,
      firmStep: FirmEconomics.StepOutput,
  )(using p: SimParams): Output =
    val expectedSectorCount = p.sectorDefs.length
    if sectorMults.length != expectedSectorCount then
      throw IllegalArgumentException(
        s"PriceEquityEconomics.compute requires sectorMults to have $expectedSectorCount entries, got ${sectorMults.length}",
      )

    val living2           = firmStep.ioFirms.filter(Firm.isAlive)
    val nLiving           = living2.length
    val autoR             =
      if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Automated]).ratioTo(nLiving).toShare
      else Share.Zero
    val hybR              =
      if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Hybrid]).ratioTo(nLiving).toShare
      else Share.Zero
    val aggInventoryStock = living2.map(_.inventory).sumPln
    val aggGreenCapital   = living2.map(_.greenCapital).sumPln

    val euMonthly = EuFunds.monthlyTransfer(month)

    val euCofin            = EuFunds.cofinancing(euMonthly)
    val euProjectCapital   = EuFunds.capitalInvestment(euMonthly, euCofin)
    val greenDomesticGFCF  = firmStep.sumGreenInvestment * (Share.One - p.climate.greenImportShare)
    val domesticGFCF       = firmStep.sumGrossInvestment * (Share.One - p.capital.importShare) + greenDomesticGFCF
    val investmentImports  = firmStep.sumGrossInvestment * p.capital.importShare + firmStep.sumGreenInvestment * p.climate.greenImportShare
    val aggInventoryChange = firmStep.sumInventoryChange
    val realizedOutputs    =
      GdpAccounting.realizedSectorOutputs(w.priceLevel, expectedSectorCount, firmStep.ioFirms, s => sectorMults(s))
    val gdp                =
      GdpAccounting.outputBasedMonthlyGdp(realizedOutputs, aggInventoryChange)

    val newMacropru = Macroprudential.step(w.mechanisms.macropru, totalSystemLoans, gdp)

    val sectorAdoption = p.sectorDefs.indices.map { s =>
      val secFirms = living2.filter(_.sector.toInt == s)
      if secFirms.isEmpty then Share.Zero
      else
        secFirms
          .count(f => f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid])
          .ratioTo(secFirms.length)
          .toShare
    }.toVector
    val baseSigmas     = p.sectorDefs.map(_.sigma).toVector
    val newSigmas      =
      evolveSigmas(w.currentSigmas, baseSigmas, sectorAdoption, p.firm.sigmaLambda, p.firm.sigmaCapMult)

    val exDev    = w.forex.exchangeRate.deviationFrom(p.forex.baseExRate)
    val priceUpd = PriceLevel.update(
      w.mechanisms.expectations.expectedInflation,
      w.priceLevel,
      avgDemandMult,
      wageGrowth,
      exDev,
    )
    // Calvo markup contribution (already annualized Rate from FirmProcessingStep)
    val newInfl  = priceUpd.inflation + firmStep.markupInflation
    val newPrice = priceUpd.priceLevel.applyGrowth(firmStep.markupInflation.monthly.toCoefficient)

    val prevGdp             = w.cachedMonthlyGdpProxy.max(PLN(1))
    val deficitToGdp        = fiscalDeficitToGdp(w.gov.deficit.max(PLN.Zero), prevGdp)
    val firmProfitsPnl      = firmStep.sumRealizedPostTaxProfit
    val gdpGrowthForEquity  = gdp.ratioTo(prevGdp).toMultiplier.deviationFromOne
    val equityAfterIndex    = EquityMarket.step(
      EquityMarket.StepInput(
        prev = w.financialMarkets.equity,
        refRate = w.nbp.referenceRate,
        inflation = newInfl,
        gdpGrowth = gdpGrowthForEquity,
        firmProfits = firmProfitsPnl,
      ),
    )
    val equityAfterIssuance = EquityMarket.processIssuance(firmStep.sumEquityIssuance, equityAfterIndex)

    val dividends              =
      EquityMarket.computeDividends(
        firmProfitsPnl,
        equityAfterIssuance.foreignOwnership,
        stateOwnedProfits = firmStep.sumStateOwnedPostTaxProfit,
        deficitToGdp = deficitToGdp,
      )
    val netDomesticDividends   = dividends.netDomestic
    val foreignDividendOutflow = dividends.foreign
    val dividendTax            = dividends.tax
    val stateOwnedGovDividends = dividends.gov

    Output(
      autoR,
      hybR,
      aggInventoryStock,
      aggGreenCapital,
      euMonthly,
      euCofin,
      euProjectCapital,
      gdp,
      realizedOutputs,
      newMacropru,
      newSigmas,
      newInfl,
      newPrice,
      equityAfterIssuance,
      netDomesticDividends,
      foreignDividendOutflow,
      dividendTax,
      stateOwnedGovDividends,
      firmProfitsPnl,
      domesticGFCF,
      investmentImports,
      aggInventoryChange,
    )
