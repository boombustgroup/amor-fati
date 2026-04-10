package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.{FirmSizeDistribution, SimParams}
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{EquityMarket, PriceLevel}
import com.boombustgroup.amorfati.engine.mechanisms.{EuFunds, Macroprudential}
import com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

/** Pure economic logic for price level dynamics, GPW equity market, GDP
  * computation, macroprudential policy, Arthur-style sigma evolution, and
  * network rewiring for bankrupt firm replacement — no state mutation, no
  * flows.
  *
  * Integrates real-side aggregates with financial market state.
  *
  * Extracted from PriceEquityStep (Calculus vs Accounting split).
  */
object PriceEquityEconomics:

  // ---- Calibration constants ----
  private val StartupCashMin    = 10000.0 // minimum startup cash for rewired entrant (PLN)
  private val StartupCashMax    = 80000.0 // maximum startup cash for rewired entrant (PLN)
  private val RiskProfileMin    = 0.1     // minimum risk profile draw for new entrant
  private val RiskProfileMax    = 0.9     // maximum risk profile draw for new entrant
  private val InnovCostMin      = 0.8     // minimum innovation cost factor draw
  private val InnovCostMax      = 1.5     // maximum innovation cost factor draw
  private val DigitalReadyFloor = 0.02    // minimum digital readiness for rewired entrant
  private val DigitalReadyCap   = 0.98    // maximum digital readiness for rewired entrant
  private val DigitalReadyNoise = 0.20    // std dev of Gaussian noise on sector base DR

  case class Input(
      w: World,                         // current world state
      month: Int,                       // month counter
      newWage: PLN,                     // new wage from labor market
      employed: Int,                    // employment count
      wageGrowth: Coefficient,          // wage growth coefficient
      domesticCons: PLN,                // domestic component of household consumption
      govPurchases: PLN,                // constrained government purchases from demand formation
      avgDemandMult: Multiplier,        // economy-wide average demand multiplier
      sectorMults: Vector[Multiplier],  // per-sector demand multipliers
      banks: Vector[Banking.BankState], // explicit bank population
      s5: FirmEconomics.StepOutput,     // firm processing output
  )

  case class Output(
      autoR: Share,
      hybR: Share,
      aggInventoryStock: PLN,
      aggGreenCapital: PLN,
      euMonthly: PLN,
      euCofin: PLN,
      euProjectCapital: PLN,
      gdp: PLN,
      newMacropru: Macroprudential.State,
      newSigmas: Vector[Sigma],
      rewiredFirms: Vector[Firm.State],
      newInfl: Rate,
      newPrice: PriceIndex,
      equityAfterIssuance: EquityMarket.State,
      netDomesticDividends: PLN,
      foreignDividendOutflow: PLN,
      dividendTax: PLN,
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
      baseSigmas: Vector[Double],
      sectorAdoption: Vector[Double],
      lambda: Double,
      capMult: Double,
  ): Vector[Sigma] =
    if lambda == 0.0 then currentSigmas
    else
      currentSigmas.zip(baseSigmas).zip(sectorAdoption).map { case ((sig, base), adopt) =>
        val s     = sig.toLong.toDouble / ScaleD
        val cap   = base * capMult
        val delta = lambda * s * adopt * (1.0 - s / cap)
        Sigma(Math.min(cap, Math.max(s, s + delta)))
      }

  // ---------------------------------------------------------------------------
  // Dynamic network rewiring — bankrupt firm replacement with preferential attachment
  // ---------------------------------------------------------------------------

  @boundaryEscape
  private[economics] def rewireFirms(firms: Vector[Firm.State], rho: Double, rng: RandomStream)(using p: SimParams): Vector[Firm.State] =
    import ComputationBoundary.toDouble
    if rho == 0.0 then return firms

    val n = firms.length
    val k = p.firm.networkK

    val toReplace = (0 until n).filter(i => !Firm.isAlive(firms(i)) && rng.nextDouble() < rho).toSet
    if toReplace.isEmpty then return firms

    val adj = Array.tabulate(n)(i => scala.collection.mutable.Set.from(firms(i).neighbors.map(_.toInt)))

    val alive = (0 until n).filter(i => Firm.isAlive(firms(i))).toArray

    for idx <- toReplace do
      for nb <- adj(idx) do adj(nb) -= idx
      adj(idx).clear()

      if alive.nonEmpty then
        val numTargets = Math.min(k, alive.length)
        val targets    = scala.collection.mutable.Set.empty[Int]
        val degrees    = alive.map(i => Math.max(1, adj(i).size))
        val totalDeg   = degrees.sum

        var attempts = 0
        while targets.size < numTargets && attempts < numTargets * 20 do
          var r = rng.nextInt(if totalDeg > 0 then totalDeg else 1)
          var j = 0
          while j < alive.length - 1 && r >= degrees(j) do
            r -= degrees(j)
            j += 1
          targets += alive(j)
          attempts += 1

        for t <- targets do
          adj(idx) += t
          adj(t) += idx

    (0 until n).map { i =>
      if toReplace.contains(i) then
        val sec      = firms(i).sector
        val newSize  = FirmSizeDistribution.draw(rng)
        val sizeMult = newSize.toDouble / p.pop.workersPerFirm
        Firm.State(
          id = FirmId(i),
          cash = PLN(rng.between(StartupCashMin, StartupCashMax) * sizeMult),
          debt = PLN.Zero,
          tech = TechState.Traditional(newSize),
          riskProfile = Share(rng.between(RiskProfileMin, RiskProfileMax)),
          innovationCostFactor = Multiplier(rng.between(InnovCostMin, InnovCostMax)),
          digitalReadiness = Share(
            Math.max(
              DigitalReadyFloor,
              Math.min(DigitalReadyCap, toDouble(p.sectorDefs(sec.toInt).baseDigitalReadiness) + (rng.nextGaussian() * DigitalReadyNoise)),
            ),
          ),
          sector = sec,
          neighbors = adj(i).iterator.map(FirmId(_)).toVector,
          bankId = BankId(0),
          equityRaised = PLN.Zero,
          initialSize = newSize,
          capitalStock = PLN(toDouble(p.capital.klRatios(sec.toInt)) * newSize),
          bondDebt = PLN.Zero,
          foreignOwned = false,
          inventory = PLN.Zero,
          greenCapital = PLN.Zero,
          accumulatedLoss = PLN.Zero,
        )
      else
        val newNb = adj(i).iterator.map(FirmId(_)).toVector
        if newNb.length != firms(i).neighbors.length then firms(i).copy(neighbors = newNb)
        else firms(i)
    }.toVector

  @boundaryEscape
  private[economics] def governmentDemandContribution(govPurchases: PLN)(using p: SimParams): Double =
    import ComputationBoundary.toDouble
    toDouble(govPurchases) * (1.0 - toDouble(p.fiscal.govInvestShare)) * toDouble(p.fiscal.govCurrentMultiplier) +
      toDouble(govPurchases) * toDouble(p.fiscal.govInvestShare) * toDouble(p.fiscal.govCapitalMultiplier)

  // ---------------------------------------------------------------------------
  // Main compute logic
  // ---------------------------------------------------------------------------

  @boundaryEscape
  def compute(in: Input, rng: RandomStream)(using p: SimParams): Output =
    import ComputationBoundary.toDouble
    val living2           = in.s5.ioFirms.filter(Firm.isAlive)
    val nLiving           = living2.length.toDouble
    val autoR             = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Automated]) / nLiving else 0.0
    val hybR              = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Hybrid]) / nLiving else 0.0
    val aggInventoryStock = PLN.fromRaw(living2.map(_.inventory.toLong).sum)
    val aggGreenCapital   = PLN.fromRaw(living2.map(_.greenCapital.toLong).sum)

    val euMonthly = EuFunds.monthlyTransfer(in.month)

    val govGdpContribution = governmentDemandContribution(in.govPurchases)
    val euCofin            = EuFunds.cofinancing(euMonthly)
    val euProjectCapital   = EuFunds.capitalInvestment(euMonthly, euCofin)
    val euGdpContribution  =
      toDouble(euProjectCapital) * toDouble(p.fiscal.govCapitalMultiplier) +
        toDouble((euCofin - euProjectCapital).max(PLN.Zero)) * toDouble(p.fiscal.govCurrentMultiplier)
    val greenDomesticGFCF  =
      toDouble(in.s5.sumGreenInvestment) * (1.0 - toDouble(p.climate.greenImportShare))
    val domesticGFCF       =
      toDouble(in.s5.sumGrossInvestment) * (1.0 - toDouble(p.capital.importShare)) + greenDomesticGFCF
    val investmentImports  =
      toDouble(in.s5.sumGrossInvestment) * toDouble(p.capital.importShare) +
        toDouble(in.s5.sumGreenInvestment) * toDouble(p.climate.greenImportShare)
    val aggInventoryChange = toDouble(in.s5.sumInventoryChange)
    val gdp                =
      toDouble(in.domesticCons) + govGdpContribution + euGdpContribution + toDouble(in.w.forex.exports) + domesticGFCF + aggInventoryChange

    val totalSystemLoans = PLN.fromRaw(in.banks.map(_.loans.toLong).sum)
    val newMacropru      = Macroprudential.step(in.w.mechanisms.macropru, totalSystemLoans, PLN(gdp))

    val sectorAdoption = p.sectorDefs.indices.map { s =>
      val secFirms = living2.filter(_.sector.toInt == s)
      if secFirms.isEmpty then 0.0
      else
        secFirms
          .count(f => f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid])
          .toDouble / secFirms.length
    }.toVector
    val baseSigmas     = p.sectorDefs.map(sd => toDouble(sd.sigma)).toVector
    val newSigmas      =
      evolveSigmas(in.w.currentSigmas, baseSigmas, sectorAdoption, toDouble(p.firm.sigmaLambda), toDouble(p.firm.sigmaCapMult))

    val rewiredFirms = rewireFirms(in.s5.ioFirms, toDouble(p.firm.rewireRho), rng)

    val exDev    = in.w.forex.exchangeRate.deviationFrom(p.forex.baseExRate)
    val priceUpd = PriceLevel.update(
      in.w.inflation,
      in.w.priceLevel,
      in.avgDemandMult,
      in.wageGrowth,
      exDev,
    )
    // Calvo markup contribution (already annualized Rate from FirmProcessingStep)
    val newInfl  = priceUpd.inflation + in.s5.markupInflation
    val newPrice = priceUpd.priceLevel.applyGrowth(in.s5.markupInflation.monthly.toCoefficient)

    val firmProfitsPnl = in.s5.sumRealizedPostTaxProfit

    val prevGdp             = in.w.cachedMonthlyGdpProxy.max(PLN(1.0))
    val gdpGrowthForEquity  = PLN(gdp).ratioTo(prevGdp).toMultiplier.deviationFromOne
    val equityAfterIndex    = EquityMarket.step(
      EquityMarket.StepInput(
        prev = in.w.financial.equity,
        refRate = in.w.nbp.referenceRate,
        inflation = newInfl,
        gdpGrowth = gdpGrowthForEquity,
        firmProfits = firmProfitsPnl,
      ),
    )
    val equityAfterIssuance = EquityMarket.processIssuance(in.s5.sumEquityIssuance, equityAfterIndex)

    val dividends              =
      EquityMarket.computeDividends(
        firmProfitsPnl,
        equityAfterIssuance.foreignOwnership,
      )
    val netDomesticDividends   = dividends.netDomestic
    val foreignDividendOutflow = dividends.foreign
    val dividendTax            = dividends.tax

    Output(
      Share(autoR),
      Share(hybR),
      aggInventoryStock,
      aggGreenCapital,
      euMonthly,
      euCofin,
      euProjectCapital,
      PLN(gdp),
      newMacropru,
      newSigmas,
      rewiredFirms,
      newInfl,
      newPrice,
      equityAfterIssuance,
      netDomesticDividends,
      foreignDividendOutflow,
      dividendTax,
      firmProfitsPnl,
      PLN(domesticGFCF),
      PLN(investmentImports),
      PLN(aggInventoryChange),
    )
