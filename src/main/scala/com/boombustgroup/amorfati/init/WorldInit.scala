package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.*
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.ledger.{CorporateBondOwnership, LedgerFinancialState}
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, EquityMarket, FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.engine.mechanisms.{Macroprudential, SectoralMobility}
import com.boombustgroup.amorfati.types.*

/** Orchestrates all initialization factories and assembles World. */
object WorldInit:

  /** Initialize a complete simulation world from an explicit randomness
    * contract.
    */
  @boundaryEscape
  def initialize(randomness: InitRandomness.Contract)(using p: SimParams): InitResult =
    import ComputationBoundary.toDouble

    // --- Firms ---
    val initCorporateBonds      = CorporateBondMarket.initial
    val initCorporateBondStocks = CorporateBondMarket.initialStock
    val firms                   = FirmInit.create(randomness.firms.newStreams())
    val initFirmBalances        = CorporateBondOwnership.initializeIssuerBalances(firms, initCorporateBondStocks.outstanding)
    assert(firms.length == p.pop.firmsCount)

    // --- Households ---
    val households0    = Household.Init.create(randomness.households.newStreams(), firms)
    val households     = ImmigrantInit.create(randomness.immigration.newStreams(), households0)
    val totalPop       = households.length
    val initEmployed   = households.count(_.status.isInstanceOf[HhStatus.Employed])
    val initUnemployed = totalPop - initEmployed
    assert(totalPop > 0)

    // --- Banking sector ---
    // Steady-state consumption estimate: employed × wage × MPC × domestic share
    val initWageBill     = initEmployed.toDouble * toDouble(p.household.baseWage)
    val initMpc          = toDouble(p.household.mpcAlpha) / (toDouble(p.household.mpcAlpha) + toDouble(p.household.mpcBeta)) // Beta mean
    val initConsumption  = PLN(initWageBill * initMpc)
    val initDomesticCons = initConsumption * Share(1.0 - p.openEcon.importContent.map(toDouble(_)).max)
    val initImportCons   = initConsumption - initDomesticCons

    val initBankingSector = BankInit.create(firms, households)
    val initBankCorpBonds =
      com.boombustgroup.ledger.Distribute
        .distribute(
          initCorporateBondStocks.bankHoldings.toLong,
          Banking.DefaultConfigs.map(_.initMarketShare.toLong).toArray,
        )
        .map(PLN.fromRaw)
        .toVector
    val initBankBalances  =
      initBankingSector.banks.map: bank =>
        Banking.FinancialBalances.fromState(bank, corpBond = initBankCorpBonds.lift(bank.id.toInt).getOrElse(PLN.Zero))

    // --- Sub-state initializers ---
    val initDemographics      = DemographicsInit.create(totalPop)
    val initInsuranceState    = InsuranceInit.create()
    val initNbfiState         = NbfiInit.create()
    val initInsuranceBalances = Insurance.initialBalances
    val initNbfiBalances      = Nbfi.initialBalances
    val initExpectations      = ExpectationsInit.create()
    val initInflation         = p.monetary.targetInfl
    val initForeignGovBonds   = PLN.Zero

    val initBondsOutstanding = p.banking.initGovBonds + p.banking.initNbpGovBonds +
      initInsuranceBalances.govBondHoldings + initNbfiBalances.tfiGovBondHoldings
    val initSocialState      = SocialState.zero.copy(demographics = initDemographics)

    // --- Steady-state gross investment ---
    val initGrossInvestment = PLN.fromRaw(firms.map(f => (f.capitalStock * p.capital.depRates(f.sector.toInt).monthly).toLong).sum)
    val initGreenInvestment = PLN.fromRaw(firms.map(f => (f.greenCapital * p.climate.greenDepRate.monthly).toLong).sum)

    // --- World assembly ---
    val initHhAgg = Household.Aggregates(
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
      meanSkill = Share.Zero,
      meanHealthPenalty = Share.Zero,
      retrainingAttempts = 0,
      retrainingSuccesses = 0,
      consumptionP10 = PLN.Zero,
      consumptionP50 = PLN.Zero,
      consumptionP90 = PLN.Zero,
      meanMonthsToRuin = Scalar.Zero,
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
    )

    val world = World(
      inflation = initInflation,
      priceLevel = PriceIndex(1.0),
      currentSigmas = p.sectorDefs.map(_.sigma),
      gov = FiscalBudget.GovState(
        taxRevenue = PLN.Zero,
        deficit = PLN.Zero,
        cumulativeDebt = p.fiscal.initGovDebt,
        unempBenefitSpend = PLN.Zero,
        bondsOutstanding = initBondsOutstanding,
        foreignBondHoldings = initForeignGovBonds,
      ),
      nbp = Nbp.State(
        referenceRate = p.monetary.initialRate,
        govBondHoldings = p.banking.initNbpGovBonds,
        qeActive = false,
        qeCumulative = PLN.Zero,
        fxReserves = p.monetary.fxReserves,
        lastFxTraded = PLN.Zero,
      ),
      bankingSector = initBankingSector.market,
      forex = OpenEconomy.ForexState(
        exchangeRate = p.forex.baseExRate,
        imports = PLN.Zero,
        exports = p.openEcon.exportBase,
        tradeBalance = PLN.Zero,
        techImports = PLN.Zero,
      ),
      householdMarket = HouseholdMarketState(
        marketWage = p.household.baseWage,
        reservationWage = p.household.baseReservationWage,
      ),
      social = initSocialState,
      financialMarkets = FinancialMarketsState(
        equity = EquityMarket.initial,
        corporateBonds = initCorporateBonds,
        insurance = initInsuranceState,
        nbfi = initNbfiState,
        quasiFiscal = QuasiFiscal.State.zero,
      ),
      external = ExternalState(
        gvc = GvcInit.create(),
        immigration = Immigration.State(p.immigration.initStock, 0, 0, PLN.Zero),
      ),
      real = RealState(
        housing = HousingInit.create(),
        sectoralMobility = SectoralMobility.zero,
        grossInvestment = initGrossInvestment,
        aggGreenInvestment = initGreenInvestment,
      ),
      mechanisms = MechanismsState(
        macropru = Macroprudential.State.zero,
        expectations = initExpectations,
      ),
      plumbing = MonetaryPlumbingState.zero,
      pipeline = PipelineState.bootstrap(
        p.sectorDefs.length,
        if initDemographics.workingAgePop > 0 then Share.One - Share.fraction(initEmployed, initDemographics.workingAgePop) else Share.Zero,
        initInflation,
        initExpectations.expectedInflation,
      ),
      flows = FlowState(monthlyGdpProxy = PLN(toDouble(p.firm.baseRevenue) * p.pop.firmsCount)),
      regionalWages = Region.all.map(r => r -> (p.household.baseWage * Region.normalizedWageMultiplier(r))).toMap,
    )

    val ledgerFinancialState = LedgerFinancialState(
      households = households.map(Household.FinancialBalances.fromState).map(LedgerFinancialState.householdBalances),
      firms = initFirmBalances,
      banks = LedgerFinancialState.refreshBankFinancialBalances(initBankBalances),
      government = LedgerFinancialState.GovernmentBalances(govBondOutstanding = initBondsOutstanding),
      foreign = LedgerFinancialState.ForeignBalances(govBondHoldings = initForeignGovBonds),
      nbp = LedgerFinancialState.nbpBalances(
        Nbp.FinancialBalances(
          govBondHoldings = p.banking.initNbpGovBonds,
          foreignAssets = p.monetary.fxReserves,
        ),
      ),
      insurance = LedgerFinancialState.insuranceBalances(initInsuranceBalances, initCorporateBondStocks.insuranceHoldings),
      funds = LedgerFinancialState.fundBalances(initSocialState, initCorporateBondStocks, initNbfiBalances, QuasiFiscal.StockState.zero),
    )

    InitResult(world, firms, households, initBankingSector.banks, initHhAgg, ledgerFinancialState)

  case class InitResult(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
      ledgerFinancialState: LedgerFinancialState,
  )
