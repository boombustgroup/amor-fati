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
    val firms0                  = FirmInit.create(randomness.firms.newStreams())
    val firms                   = CorporateBondOwnership.initializeIssuerDebt(firms0, initCorporateBondStocks.outstanding)
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

    val initBankingSector = BankInit.create(firms, households, initCorporateBondStocks.bankHoldings)

    // --- Sub-state initializers ---
    val initDemographics   = DemographicsInit.create(totalPop)
    val initInsurance      = InsuranceInit.create()
    val initNbfi           = NbfiInit.create()
    val initInsuranceStock = CorporateBondOwnership.alignInsuranceStock(Insurance.initialStock, initCorporateBondStocks.insuranceHoldings)
    val initNbfiStock      = CorporateBondOwnership.alignNbfiStock(Nbfi.initialStock, initCorporateBondStocks.nbfiHoldings)
    val initExpectations   = ExpectationsInit.create()
    val initInflation      = p.monetary.targetInfl

    val initBondsOutstanding = p.banking.initGovBonds + p.banking.initNbpGovBonds +
      initInsuranceStock.govBondHoldings + initNbfiStock.tfiGovBondHoldings

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
      householdMarket = HouseholdMarketState(
        marketWage = p.household.baseWage,
        reservationWage = p.household.baseReservationWage,
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
        equity = EquityMarket.initial,
        corporateBonds = initCorporateBonds,
        insurance = initInsurance,
        nbfi = initNbfi,
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
      households = households.map(LedgerFinancialState.householdBalances),
      firms = firms.map(LedgerFinancialState.firmBalances),
      banks = initBankingSector.banks.map(LedgerFinancialState.bankBalances),
      government = LedgerFinancialState.governmentBalances(world.gov),
      foreign = LedgerFinancialState.foreignBalances(world.gov),
      nbp = LedgerFinancialState.nbpBalances(world.nbp),
      insurance = LedgerFinancialState.insuranceBalances(initInsuranceStock),
      funds = LedgerFinancialState.fundBalances(world.social, initCorporateBondStocks, initNbfiStock, QuasiFiscal.StockState.zero),
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
