package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.*
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.engine.mechanisms.{Macroprudential, SectoralMobility}
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Orchestrates all initialization factories and assembles World. */
object WorldInit:

  /** Initialize a complete simulation world from a seed. */
  @boundaryEscape
  def initialize(seed: Long)(using p: SimParams): InitResult =
    import ComputationBoundary.toDouble
    val rng = new Random(seed)

    // --- Firms ---
    val firms = FirmInit.create(rng)
    assert(firms.length == p.pop.firmsCount)

    // --- Households ---
    val households0    = Household.Init.create(rng, firms)
    val households     = ImmigrantInit.create(rng, households0)
    val totalPop       = households.length
    val initEmployed   = households.count(_.status.isInstanceOf[HhStatus.Employed])
    val initUnemployed = totalPop - initEmployed
    assert(totalPop > 0)

    // --- Banking sector ---
    // Steady-state consumption estimate: employed × wage × MPC × domestic share
    val initWageBill     = initEmployed.toDouble * toDouble(p.household.baseWage)
    val initMpc          = p.household.mpcAlpha / (p.household.mpcAlpha + p.household.mpcBeta) // Beta mean
    val initConsumption  = PLN(initWageBill * initMpc)
    val initDomesticCons = initConsumption * Share(1.0 - p.openEcon.importContent.map(toDouble(_)).max)
    val initImportCons   = initConsumption - initDomesticCons

    val initBankingSector = BankInit.create(firms, households)

    // --- Sub-state initializers ---
    val initDemographics = DemographicsInit.create(totalPop)
    val initInsurance    = InsuranceInit.create()
    val initNbfi         = NbfiInit.create()

    val initBondsOutstanding = p.banking.initGovBonds + p.banking.initNbpGovBonds +
      initInsurance.govBondHoldings + initNbfi.tfiGovBondHoldings

    // --- Steady-state gross investment ---
    val initGrossInvestment =
      if p.flags.physCap then PLN.fromRaw(firms.map(f => (f.capitalStock * p.capital.depRates(f.sector.toInt).monthly).toLong).sum)
      else PLN.Zero
    val initGreenInvestment =
      if p.flags.energy then PLN.fromRaw(firms.map(f => (f.greenCapital * p.climate.greenDepRate.monthly).toLong).sum)
      else PLN.Zero

    // --- World assembly ---
    val world = World(
      month = 0,
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gdpProxy = toDouble(p.firm.baseRevenue) * p.pop.firmsCount,
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
      bankingSector = initBankingSector.marketState,
      forex = OpenEconomy.ForexState(
        exchangeRate = p.forex.baseExRate,
        imports = PLN.Zero,
        exports = p.openEcon.exportBase,
        tradeBalance = PLN.Zero,
        techImports = PLN.Zero,
      ),
      hhAgg = Household.Aggregates(
        employed = initEmployed,
        unemployed = initUnemployed,
        retraining = 0,
        bankrupt = 0,
        totalIncome = PLN(initWageBill),
        consumption = initConsumption,
        domesticConsumption = initDomesticCons,
        importConsumption = initImportCons,
        marketWage = p.household.baseWage,
        reservationWage = p.household.baseReservationWage,
        giniIndividual = Share.Zero,
        giniWealth = Share.Zero,
        meanSavings = PLN.Zero,
        medianSavings = PLN.Zero,
        povertyRate50 = Share.Zero,
        bankruptcyRate = Share.Zero,
        meanSkill = 0.0,
        meanHealthPenalty = 0.0,
        retrainingAttempts = 0,
        retrainingSuccesses = 0,
        consumptionP10 = PLN.Zero,
        consumptionP50 = PLN.Zero,
        consumptionP90 = PLN.Zero,
        meanMonthsToRuin = 0.0,
        povertyRate30 = Share.Zero,
        totalRent = PLN.Zero,
        totalDebtService = PLN.Zero,
        totalUnempBenefits = PLN.Zero,
        totalDepositInterest = PLN.Zero,
        crossSectorHires = 0,
        voluntaryQuits = 0,
        sectorMobilityRate = Share.Zero,
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
        earmarked = EarmarkedFunds.State.zero,
      ),
      financial = FinancialMarketsState(
        equity = EquityInit.create(totalPop),
        corporateBonds = CorporateBondMarket.initial,
        insurance = initInsurance,
        nbfi = initNbfi,
        quasiFiscal = QuasiFiscal.State.zero,
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
      regionalWages = Region.all.map(r => r -> (p.household.baseWage * Region.normalizedWageMultiplier(r))).toMap,
    )

    InitResult(world, firms, households, initBankingSector.banks)

  case class InitResult(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
  )
