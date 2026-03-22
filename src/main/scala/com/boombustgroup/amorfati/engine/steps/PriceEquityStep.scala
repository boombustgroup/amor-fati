package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.{FirmSizeDistribution, SimParams}
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{EquityMarket, PriceLevel}
import com.boombustgroup.amorfati.engine.mechanisms.{EuFunds, Macroprudential}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

import scala.util.Random

/** Price level dynamics, GPW equity market, GDP computation, macroprudential
  * policy, Arthur-style sigma evolution, and network rewiring for bankrupt firm
  * replacement. Integrates real-side aggregates with financial market state.
  */
object PriceEquityStep:

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
  private val AiMaintRealShare  = 0.60    // real (price-insensitive) share of AI/hybrid maintenance cost
  private val AiMaintNomShare   = 0.40    // nominal (price-sensitive) share of AI/hybrid maintenance cost

  case class Input(
      w: World,                         // current world state
      s1: FiscalConstraintStep.Output,  // fiscal constraint (month counter)
      s2: LaborDemographicsStep.Output, // labor/demographics (employment, wage, living firms)
      s3: HouseholdIncomeStep.Output,   // household income (domestic consumption)
      s4: DemandStep.Output,            // demand (sector multipliers, gov purchases)
      s5: FirmProcessingStep.Output,    // firm processing (I-O firms, equity issuance, investment)
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
      newPrice: Double,
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
  private[steps] def evolveSigmas(
      currentSigmas: Vector[Sigma],
      baseSigmas: Vector[Double],
      sectorAdoption: Vector[Double],
      lambda: Double,
      capMult: Double,
  ): Vector[Sigma] =
    // Fast path: when lambda=0 the mechanism is disabled — sigma is static across the entire simulation.
    // This is the default for baseline runs and backward-compatible experiments.
    if lambda == 0.0 then currentSigmas
    else
      currentSigmas.zip(baseSigmas).zip(sectorAdoption).map { case ((sig, base), adopt) =>
        val s     = sig.toDouble
        val cap   = base * capMult
        // Logistic delta: positive when s < cap and adopt > 0; approaches zero as s → cap.
        val delta = lambda * s * adopt * (1.0 - s / cap)
        // Ratchet (max with current) ensures sigma never decreases; hard cap ensures it never overshoots.
        Sigma(Math.min(cap, Math.max(s, s + delta)))
      }

  // ---------------------------------------------------------------------------
  // Dynamic network rewiring — bankrupt firm replacement with preferential attachment
  // ---------------------------------------------------------------------------
  //
  // In real economies, bankrupt firms don't leave permanent "holes" in the production
  // network — their market niches get filled by new entrants. This mechanism models
  // that process: each bankrupt firm has probability rho of being replaced each month
  // by a fresh Traditional entrant.
  //
  // New entrants:
  //   - Inherit the **same sector** as the bankrupt firm (market niche persistence)
  //   - Start with fresh financial state: random cash, zero debt, Traditional tech
  //   - Firm size drawn from FirmSizeDistribution (matching the empirical GUS distribution)
  //   - Connect to k alive firms via **preferential attachment** (Barabási-Albert mechanism):
  //     connection probability proportional to current degree, so well-connected hub firms
  //     attract more new partners — mimicking real-world "rich get richer" network formation
  //
  // When rho=0.0, the function is a no-op (static network mode). This is the default
  // for experiments that need a fixed network topology.
  //
  // The mechanism interacts with Firm.computeLocalAutoRatio (technology diffusion via network
  // peer effects): newly wired entrants immediately start receiving competitive pressure
  // from their automated neighbors.
  //
  // Reference: Barabási, A.-L. & Albert, R. (1999), "Emergence of Scaling in Random
  // Networks", Science 286(5439).
  // ---------------------------------------------------------------------------

  /** Replace bankrupt firms with new Traditional entrants, wired via
    * preferential attachment.
    *
    * Each bankrupt firm has probability rho of being replaced each step. New
    * entrants inherit the same sector, start with fresh state, and connect to k
    * alive firms weighted by degree (preferential attachment).
    *
    * When rho=0.0, returns firms unchanged (static mode = no-op).
    *
    * @param firms
    *   current firm array (some may be Bankrupt)
    * @param rho
    *   replacement probability per bankrupt firm per step (0.0 = static)
    * @return
    *   updated firm array with same length
    */
  private[steps] def rewireFirms(firms: Vector[Firm.State], rho: Double, rng: Random)(using p: SimParams): Vector[Firm.State] =
    // Fast path: static network mode — no rewiring, return the exact same array instance.
    if rho == 0.0 then return firms

    val n = firms.length
    val k = p.firm.networkK

    // Identify bankrupt firms eligible for replacement. Each bankrupt firm independently
    // "rolls the dice" with probability rho — this creates stochastic variation in the
    // replacement rate, avoiding artificial synchronization of firm entry.
    val toReplace = (0 until n).filter(i => !Firm.isAlive(firms(i)) && rng.nextDouble() < rho).toSet
    if toReplace.isEmpty then return firms

    // Build mutable adjacency from current neighbor arrays so we can rewire edges
    // without modifying the original firm states.
    val adj = Array.tabulate(n)(i => scala.collection.mutable.Set.from(firms(i).neighbors.map(_.toInt)))

    // Alive firm indices serve as targets for preferential attachment.
    val alive = (0 until n).filter(i => Firm.isAlive(firms(i))).toArray

    for idx <- toReplace do
      // Remove all edges of the bankrupt firm — its old connections are severed.
      for nb <- adj(idx) do adj(nb) -= idx
      adj(idx).clear()

      // Wire the new entrant to k existing alive firms via preferential attachment:
      // probability of connecting to firm j is proportional to deg(j).
      if alive.nonEmpty then
        val numTargets = Math.min(k, alive.length)
        val targets    = scala.collection.mutable.Set.empty[Int]
        val degrees    = alive.map(i => Math.max(1, adj(i).size))
        val totalDeg   = degrees.sum

        // Retry loop with bounded attempts to avoid infinite loops in degenerate cases
        // (e.g., very small networks where all targets are already selected).
        var attempts = 0
        while targets.size < numTargets && attempts < numTargets * 20 do
          var r = rng.nextInt(if totalDeg > 0 then totalDeg else 1)
          var j = 0
          while j < alive.length - 1 && r >= degrees(j) do
            r -= degrees(j)
            j += 1
          targets += alive(j)
          attempts += 1

        // Create symmetric edges (undirected graph).
        for t <- targets do
          adj(idx) += t
          adj(t) += idx

    // Rebuild firm array: replaced firms get fresh state, others get updated neighbor lists.
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
          innovationCostFactor = rng.between(InnovCostMin, InnovCostMax),
          digitalReadiness = Share(
            Math.max(
              DigitalReadyFloor,
              Math.min(DigitalReadyCap, p.sectorDefs(sec.toInt).baseDigitalReadiness.toDouble + (rng.nextGaussian() * DigitalReadyNoise)),
            ),
          ),
          sector = sec,
          neighbors = adj(i).iterator.map(FirmId(_)).toVector,
          bankId = BankId(0),
          equityRaised = PLN.Zero,
          initialSize = newSize,
          capitalStock = if p.flags.physCap then p.capital.klRatios(sec.toInt) * newSize.toDouble else PLN.Zero,
          bondDebt = PLN.Zero,
          foreignOwned = false,
          inventory = PLN.Zero,
          greenCapital = PLN.Zero,
          accumulatedLoss = PLN.Zero,
        )
      else
        // For surviving firms: only allocate a new neighbors array if the neighbor set actually changed
        // (an edge was added/removed due to a nearby firm being replaced). This avoids unnecessary
        // garbage collection pressure in the common case where most firms are unaffected.
        val newNb = adj(i).iterator.map(FirmId(_)).toVector
        if newNb.length != firms(i).neighbors.length then firms(i).copy(neighbors = newNb)
        else firms(i)
    }.toVector

  // ---------------------------------------------------------------------------
  // Main step logic
  // ---------------------------------------------------------------------------

  def run(in: Input, rng: Random)(using p: SimParams): Output =
    val living2           = in.s5.ioFirms.filter(Firm.isAlive)
    val nLiving           = living2.length.toDouble
    val autoR             = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Automated]) / nLiving else 0.0
    val hybR              = if nLiving > 0 then living2.count(_.tech.isInstanceOf[TechState.Hybrid]) / nLiving else 0.0
    val aggInventoryStock = if p.flags.inventory then living2.kahanSumBy(_.inventory.toDouble) else 0.0
    val aggGreenCapital   = if p.flags.energy then living2.kahanSumBy(_.greenCapital.toDouble) else 0.0

    val euMonthly =
      if p.flags.euFunds then EuFunds.monthlyTransfer(in.s1.m)
      else p.openEcon.euTransfers.toDouble

    val govGdpContribution =
      if p.flags.govInvest then
        p.fiscal.govBaseSpending.toDouble * (1.0 - p.fiscal.govInvestShare.toDouble) * p.fiscal.govCurrentMultiplier.toDouble +
          p.fiscal.govBaseSpending.toDouble * p.fiscal.govInvestShare.toDouble * p.fiscal.govCapitalMultiplier.toDouble
      else p.fiscal.govBaseSpending.toDouble
    val euCofin            = if p.flags.euFunds then EuFunds.cofinancing(euMonthly) else 0.0
    val euProjectCapital   =
      if p.flags.euFunds && p.flags.govInvest then EuFunds.capitalInvestment(euMonthly, euCofin)
      else 0.0
    val euGdpContribution  =
      if p.flags.euFunds && p.flags.govInvest then
        euProjectCapital * p.fiscal.govCapitalMultiplier.toDouble +
          (euCofin - euProjectCapital).max(0.0) * p.fiscal.govCurrentMultiplier.toDouble
      else if p.flags.euFunds then euCofin
      else 0.0
    val greenDomesticGFCF  =
      if p.flags.energy then in.s5.sumGreenInvestment.toDouble * (1.0 - p.climate.greenImportShare.toDouble) else 0.0
    val domesticGFCF       = (if p.flags.physCap then in.s5.sumGrossInvestment.toDouble * (1.0 - p.capital.importShare.toDouble)
                        else 0.0) + greenDomesticGFCF
    val investmentImports  =
      (if p.flags.physCap then in.s5.sumGrossInvestment.toDouble * p.capital.importShare.toDouble else 0.0) +
        (if p.flags.energy then in.s5.sumGreenInvestment.toDouble * p.climate.greenImportShare.toDouble else 0.0)
    val aggInventoryChange = if p.flags.inventory then in.s5.sumInventoryChange.toDouble else 0.0
    val gdp                =
      in.s3.domesticCons.toDouble + govGdpContribution + euGdpContribution + in.w.forex.exports.toDouble + domesticGFCF + aggInventoryChange

    val totalSystemLoans = in.w.bankingSector.banks.kahanSumBy(_.loans.toDouble)
    val newMacropru      = Macroprudential.step(in.w.mechanisms.macropru, totalSystemLoans, gdp)

    val sectorAdoption = p.sectorDefs.indices.map { s =>
      val secFirms = living2.filter(_.sector.toInt == s)
      if secFirms.isEmpty then 0.0
      else
        secFirms
          .count(f => f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid])
          .toDouble / secFirms.length
    }.toVector
    val baseSigmas     = p.sectorDefs.map(_.sigma.toDouble).toVector
    val newSigmas      =
      evolveSigmas(in.w.currentSigmas, baseSigmas, sectorAdoption, p.firm.sigmaLambda, p.firm.sigmaCapMult.toDouble)

    val rewiredFirms = rewireFirms(in.s5.ioFirms, p.firm.rewireRho.toDouble, rng)

    val exDev    = (in.w.forex.exchangeRate / p.forex.baseExRate) - 1.0
    val priceUpd = PriceLevel.update(
      in.w.inflation,
      in.w.priceLevel,
      in.s4.avgDemandMult,
      in.s2.wageGrowth.toDouble,
      exDev,
      autoR,
      hybR,
    )
    // Calvo markup contribution (already annualized Rate from FirmProcessingStep)
    val newInfl  = (priceUpd.inflation + in.s5.markupInflation).toDouble
    val newPrice = priceUpd.priceLevel * (1.0 + in.s5.markupInflation.monthly.toDouble)

    val firmProfits = living2.kahanSumBy { f =>
      val rev      = Firm.computeCapacity(f).toDouble * in.s4.sectorMults(f.sector.toInt) * newPrice
      val labor    = Firm.workerCount(f) * in.s2.newWage.toDouble * p.sectorDefs(f.sector.toInt).wageMultiplier.toDouble
      val other    = p.firm.otherCosts.toDouble * newPrice
      val aiMaint  = f.tech match
        case _: TechState.Automated => p.firm.aiOpex.toDouble * (AiMaintRealShare + AiMaintNomShare * newPrice)
        case _: TechState.Hybrid    => p.firm.hybridOpex.toDouble * (AiMaintRealShare + AiMaintNomShare * newPrice)
        case _                      => 0.0
      val interest = f.debt.toDouble * in.s5.lendingRates(f.bankId.toInt) / 12.0
      val gross    = rev - labor - other - aiMaint - interest
      val tax      = Math.max(0.0, gross) * p.fiscal.citRate.toDouble
      Math.max(0.0, gross - tax)
    }

    val prevGdp             = if in.w.gdpProxy > 0 then in.w.gdpProxy else 1.0
    val gdpGrowthForEquity  = (gdp - prevGdp) / prevGdp
    val equityAfterIndex    = EquityMarket.step(
      EquityMarket.StepInput(
        prev = in.w.financial.equity,
        refRate = in.w.nbp.referenceRate,
        inflation = Rate(newInfl),
        gdpGrowth = gdpGrowthForEquity,
        firmProfits = PLN(firmProfits),
      ),
    )
    val equityAfterIssuance = EquityMarket.processIssuance(in.s5.sumEquityIssuance, equityAfterIndex)

    val dividends              =
      if p.flags.gpw && p.flags.gpwDividends then
        EquityMarket.computeDividends(
          equityAfterIssuance.dividendYield,
          equityAfterIssuance.marketCap,
          equityAfterIssuance.foreignOwnership,
        )
      else EquityMarket.DividendResultZero
    val netDomesticDividends   = dividends.netDomestic.toDouble
    val foreignDividendOutflow = dividends.foreign.toDouble
    val dividendTax            = dividends.tax.toDouble

    Output(
      Share(autoR),
      Share(hybR),
      PLN(aggInventoryStock),
      PLN(aggGreenCapital),
      PLN(euMonthly),
      PLN(euCofin),
      PLN(euProjectCapital),
      PLN(gdp),
      newMacropru,
      newSigmas,
      rewiredFirms,
      Rate(newInfl),
      newPrice,
      equityAfterIssuance,
      PLN(netDomesticDividends),
      PLN(foreignDividendOutflow),
      PLN(dividendTax),
      PLN(firmProfits),
      PLN(domesticGFCF),
      PLN(investmentImports),
      PLN(aggInventoryChange),
    )
