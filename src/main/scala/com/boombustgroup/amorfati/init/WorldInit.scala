package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.*
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.engine.mechanisms.{Macroprudential, SectoralMobility}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

import scala.util.Random

/** Orchestrates all initialization factories and assembles World. */
object WorldInit:

  /** Initialize a complete simulation world from a seed. */
  def initialize(seed: Long)(using p: SimParams): InitResult =
    val rng = new Random(seed)

    // --- Firms ---
    val firms = FirmInit.create(rng)
    assert(firms.length == p.pop.firmsCount)

    // --- Households ---
    val households0 = Household.Init.create(rng, firms)
    val households  = ImmigrantInit.create(rng, households0)
    val totalPop    = households.length
    assert(totalPop > 0)

    // --- Banking sector ---
    val initBankingSector = BankInit.create(firms, households)

    // --- Sub-state initializers ---
    val initDemographics = DemographicsInit.create(totalPop)
    val initInsurance    = InsuranceInit.create()
    val initNbfi         = NbfiInit.create()

    val initBondsOutstanding = p.banking.initGovBonds + p.banking.initNbpGovBonds +
      initInsurance.govBondHoldings + initNbfi.tfiGovBondHoldings

    // --- Steady-state gross investment ---
    val initGrossInvestment =
      if p.flags.physCap then PLN(firms.kahanSumBy(f => (f.capitalStock * p.capital.depRates.map(_.toDouble)(f.sector.toInt) / 12.0).toDouble))
      else PLN.Zero
    val initGreenInvestment =
      if p.flags.energy then PLN(firms.kahanSumBy(f => (f.greenCapital * p.climate.greenDepRate.toDouble / 12.0).toDouble))
      else PLN.Zero

    // --- World assembly ---
    val world = World(
      month = 0,
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gdpProxy = p.firm.baseRevenue.toDouble * p.pop.firmsCount,
      currentSigmas = p.sectorDefs.map(_.sigma),
      totalPopulation = totalPop,
      gov = FiscalBudget.GovState(
        taxRevenue = PLN.Zero,
        deficit = PLN.Zero,
        cumulativeDebt = p.fiscal.initGovDebt,
        unempBenefitSpend = PLN.Zero,
        bondsOutstanding = initBondsOutstanding,
      ),
      nbp = Nbp.State(
        referenceRate = p.monetary.initialRate,
        govBondHoldings = p.banking.initNbpGovBonds,
        qeActive = false,
        qeCumulative = PLN.Zero,
        fxReserves = p.monetary.fxReserves,
        lastFxTraded = PLN.Zero,
      ),
      bank = initBankingSector.aggregate,
      bankingSector = initBankingSector,
      forex = OpenEconomy.ForexState(
        exchangeRate = p.forex.baseExRate,
        imports = PLN.Zero,
        exports = p.forex.exportBase,
        tradeBalance = PLN.Zero,
        techImports = PLN.Zero,
      ),
      hhAgg = Household.Aggregates(
        employed = totalPop,
        unemployed = 0,
        retraining = 0,
        bankrupt = 0,
        totalIncome = PLN.Zero,
        consumption = PLN.Zero,
        domesticConsumption = PLN.Zero,
        importConsumption = PLN.Zero,
        marketWage = p.household.baseWage,
        reservationWage = p.household.baseReservationWage,
        giniIndividual = Ratio.Zero,
        giniWealth = Ratio.Zero,
        meanSavings = PLN.Zero,
        medianSavings = PLN.Zero,
        povertyRate50 = Ratio.Zero,
        bankruptcyRate = Ratio.Zero,
        meanSkill = 0.0,
        meanHealthPenalty = 0.0,
        retrainingAttempts = 0,
        retrainingSuccesses = 0,
        consumptionP10 = PLN.Zero,
        consumptionP50 = PLN.Zero,
        consumptionP90 = PLN.Zero,
        meanMonthsToRuin = 0.0,
        povertyRate30 = Ratio.Zero,
        totalRent = PLN.Zero,
        totalDebtService = PLN.Zero,
        totalUnempBenefits = PLN.Zero,
        totalDepositInterest = PLN.Zero,
        crossSectorHires = 0,
        voluntaryQuits = 0,
        sectorMobilityRate = Ratio.Zero,
        totalRemittances = PLN.Zero,
        totalPit = PLN.Zero,
        totalSocialTransfers = PLN.Zero,
        totalConsumerDebtService = PLN.Zero,
        totalConsumerOrigination = PLN.Zero,
        totalConsumerDefault = PLN.Zero,
        totalConsumerPrincipal = PLN.Zero,
      ),
      social = SocialState(
        jst = Jst.State.zero,
        zus = SocialSecurity.ZusState.zero,
        nfz = SocialSecurity.NfzState.zero,
        ppk = SocialSecurity.PpkState.zero,
        demographics = initDemographics,
      ),
      financial = FinancialMarketsState(
        equity = EquityInit.create(totalPop),
        corporateBonds = CorporateBondMarket.initial,
        insurance = initInsurance,
        nbfi = initNbfi,
      ),
      external = ExternalState(
        gvc = GvcInit.create(),
        immigration =
          if p.flags.immigration then Immigration.State(p.immigration.initStock, 0, 0, PLN.Zero)
          else Immigration.State.zero,
      ),
      real = RealState(
        housing = HousingInit.create(),
        sectoralMobility = SectoralMobility.zero,
        grossInvestment = initGrossInvestment,
        aggGreenInvestment = initGreenInvestment,
      ),
      mechanisms = MechanismsState(
        macropru = Macroprudential.State.zero,
        expectations = ExpectationsInit.create(),
      ),
      plumbing = MonetaryPlumbingState.zero,
      flows = FlowState.zero,
    )

    InitResult(world, firms, households)

  case class InitResult(world: World, firms: Vector[Firm.State], households: Vector[Household.State])
