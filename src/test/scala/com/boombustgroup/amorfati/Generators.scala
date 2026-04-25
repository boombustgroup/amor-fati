package com.boombustgroup.amorfati

import org.scalacheck.Gen
import com.boombustgroup.amorfati.accounting.*
import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.FixedPointSpecSupport.*

object Generators:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  /** Test helper: create banking sector by splitting aggregates across banks by
    * market share.
    */
  def testBankingSector(
      totalDeposits: PLN = PLN("1e9"),
      totalCapital: PLN = PLN("5e8"),
      totalLoans: PLN = PLN("5e8"),
      totalGovBonds: PLN = PLN.Zero,
      totalConsumerLoans: PLN = PLN.Zero,
      configs: Vector[Banking.Config] = Banking.DefaultConfigs,
  ): Banking.BankStockState =
    val rows = configs.map: cfg =>
      val bankBonds = totalGovBonds * cfg.initMarketShare
      (
        Banking.BankState(
          id = cfg.id,
          capital = totalCapital * cfg.initMarketShare,
          nplAmount = PLN.Zero,
          htmBookYield = p.banking.initHtmBookYield,
          status = Banking.BankStatus.Active(0),
          loansShort = PLN.Zero,
          loansMedium = PLN.Zero,
          loansLong = PLN.Zero,
          consumerNpl = PLN.Zero,
        ),
        Banking.BankFinancialStocks(
          totalDeposits = totalDeposits * cfg.initMarketShare,
          firmLoan = totalLoans * cfg.initMarketShare,
          govBondAfs = bankBonds * (Share.One - p.banking.htmShare),
          govBondHtm = bankBonds * p.banking.htmShare,
          reserve = PLN.Zero,
          interbankLoan = PLN.Zero,
          demandDeposit = PLN.Zero,
          termDeposit = PLN.Zero,
          consumerLoan = totalConsumerLoans * cfg.initMarketShare,
        ),
      )
    Banking.BankStockState(rows.map(_._1), rows.map(_._2))

  def testHouseholdAggregates(
      employed: Int = 100,
      unemployed: Int = 0,
      marketWage: PLN = PLN(8000),
      reservationWage: PLN = PLN(4666),
  ): Household.Aggregates =
    Household.Aggregates(
      employed = employed,
      unemployed = unemployed,
      retraining = 0,
      bankrupt = 0,
      totalIncome = PLN.Zero,
      consumption = PLN.Zero,
      domesticConsumption = PLN.Zero,
      importConsumption = PLN.Zero,
      marketWage = marketWage,
      reservationWage = reservationWage,
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

  def testWorld(
      inflation: Rate = Rate("0.02"),
      priceLevel: PriceIndex = PriceIndex.Base,
      monthlyGdpProxy: PLN = PLN("1e9"),
      currentSigmas: Vector[Sigma] = p.sectorDefs.map(_.sigma).toVector,
      totalPopulation: Int = 100,
      employed: Int = 100,
      marketWage: PLN = PLN(8000),
      reservationWage: PLN = PLN(4666),
      gov: FiscalBudget.GovState = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      nbp: Nbp.State = Nbp.State(Rate("0.0575"), false, PLN.Zero, PLN.Zero),
      bankingSector: Banking.MarketState = Banking.MarketState(Rate.Zero, Banking.DefaultConfigs, None),
      forex: OpenEconomy.ForexState = OpenEconomy.ForexState(ExchangeRate("4.33"), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      social: SocialState = SocialState.zero,
      financialMarkets: FinancialMarketsState = FinancialMarketsState.zero,
      external: ExternalState = ExternalState.zero,
      real: RealState = RealState.zero,
      mechanisms: MechanismsState = MechanismsState.zero,
      plumbing: MonetaryPlumbingState = MonetaryPlumbingState.zero,
      flows: FlowState = FlowState.zero,
  ): World =
    val demographics =
      if social.demographics == SocialSecurity.DemographicsState.zero && totalPopulation > 0
      then social.demographics.copy(workingAgePop = totalPopulation)
      else social.demographics
    val households   = testHouseholdAggregates(
      employed = employed,
      unemployed = (totalPopulation - employed).max(0),
      marketWage = marketWage,
      reservationWage = reservationWage,
    )
    val flowState    =
      if flows.monthlyGdpProxy == PLN.Zero && monthlyGdpProxy != PLN.Zero then flows.copy(monthlyGdpProxy = monthlyGdpProxy)
      else flows

    World(
      inflation = inflation,
      priceLevel = priceLevel,
      currentSigmas = currentSigmas,
      gov = gov,
      nbp = nbp,
      bankingSector = bankingSector,
      forex = forex,
      householdMarket = HouseholdMarketState.fromAggregates(households),
      social = social.copy(demographics = demographics),
      financialMarkets = financialMarkets,
      external = external,
      real = real,
      mechanisms = mechanisms,
      plumbing = plumbing,
      pipeline = PipelineState.bootstrap(
        currentSigmas.length,
        if totalPopulation > 0 then Share.One - Share.fraction(employed, totalPopulation) else Share.Zero,
        inflation,
        mechanisms.expectations.expectedInflation,
      ),
      flows = flowState,
    )

  // --- Primitive generators ---

  val genRate: Gen[BigDecimal] = genDecimal("0.0", "0.25")

  val genPrice: Gen[BigDecimal] = genDecimal("0.30", "5.0")

  val genExchangeRate: Gen[BigDecimal] = genDecimal("2.5", "10.0")

  val genWage: Gen[BigDecimal] = genDecimal("4666.0", "30000.0")

  val genInflation: Gen[BigDecimal] = genDecimal("-0.50", "0.50")

  val genSigma: Gen[BigDecimal] = genDecimal("0.1", "100.0")

  val genFraction: Gen[BigDecimal] = genDecimal("0.0", "1.0")

  val genPositiveDecimal: Gen[BigDecimal] = genDecimal("1.0", "1000000000.0")

  val genSmallPositiveDecimal: Gen[BigDecimal] = genDecimal("0.0", "10000000.0")

  // --- PLN generator helper ---

  val genPLN: Gen[PLN] = genDecimal("-10000000000.0", "10000000000.0").map(PLN(_))

  def genPLNRange(lo: String, hi: String): Gen[PLN] = genDecimal(lo, hi).map(PLN(_))

  // --- TechState generators ---

  val genTechState: Gen[TechState] = Gen.oneOf(
    Gen.choose(1, 20).map(w => TechState.Traditional(w)),
    for
      w   <- Gen.choose(1, 15)
      eff <- genDecimal("0.5", "2.0")
    yield TechState.Hybrid(w, Multiplier(eff)),
    genDecimal("0.5", "3.0").map(e => TechState.Automated(Multiplier(e))),
    Gen.const(TechState.Bankrupt(BankruptReason.Other("test"))),
  )

  val genAliveTechState: Gen[TechState] = Gen.oneOf(
    Gen.choose(1, 20).map(w => TechState.Traditional(w)),
    for
      w   <- Gen.choose(1, 15)
      eff <- genDecimal("0.5", "2.0")
    yield TechState.Hybrid(w, Multiplier(eff)),
    genDecimal("0.5", "3.0").map(e => TechState.Automated(Multiplier(e))),
  )

  // --- Firm generators ---

  val genFirm: Gen[Firm.State] = for
    id     <- Gen.choose(0, 9999)
    cash   <- genDecimal("-100000.0", "5000000.0")
    debt   <- genDecimal("0.0", "3000000.0")
    tech   <- genTechState
    risk   <- genFraction
    innov  <- genDecimal("0.5", "2.0")
    digiR  <- genDecimal("0.02", "0.98")
    sector <- Gen.choose(0, 5)
    bankId <- Gen.choose(0, 6)
    eqR    <- genDecimal("0.0", "1000000.0")
    iSize  <- Gen.choose(1, 500)
  yield TestFirmState(
    FirmId(id),
    PLN(cash),
    PLN(debt),
    tech,
    Share(risk),
    Multiplier(innov),
    Share(digiR),
    SectorIdx(sector),
    Vector.empty[FirmId],
    BankId(bankId),
    PLN(eqR),
    iSize,
    capitalStock = PLN.Zero,
    foreignOwned = false,
    inventory = PLN.Zero,
    greenCapital = PLN.Zero,
    accumulatedLoss = PLN.Zero,
  )

  val genAliveFirm: Gen[Firm.State] = for
    id     <- Gen.choose(0, 9999)
    cash   <- genDecimal("0.0", "5000000.0")
    debt   <- genDecimal("0.0", "3000000.0")
    tech   <- genAliveTechState
    risk   <- genFraction
    innov  <- genDecimal("0.5", "2.0")
    digiR  <- genDecimal("0.02", "0.98")
    sector <- Gen.choose(0, 5)
    bankId <- Gen.choose(0, 6)
    eqR    <- genDecimal("0.0", "1000000.0")
    iSize  <- Gen.choose(1, 500)
  yield TestFirmState(
    FirmId(id),
    PLN(cash),
    PLN(debt),
    tech,
    Share(risk),
    Multiplier(innov),
    Share(digiR),
    SectorIdx(sector),
    Vector.empty[FirmId],
    BankId(bankId),
    PLN(eqR),
    iSize,
    capitalStock = PLN.Zero,
    foreignOwned = false,
    inventory = PLN.Zero,
    greenCapital = PLN.Zero,
    accumulatedLoss = PLN.Zero,
  )

  // --- Balance sheet state generators ---

  val genBankingAggregate: Gen[Banking.Aggregate] = for
    totalLoans <- genDecimal("1000.0", "10000000000.0")
    nplFrac    <- genDecimal("0.0", "0.30")
    capital    <- genDecimal("1000.0", "1000000000.0")
    deposits   <- genDecimal("0.0", "10000000000.0")
    bonds      <- genDecimal("0.0", "1000000000.0")
    htmFrac    <- genFraction
  yield Banking.Aggregate(
    PLN(totalLoans),
    PLN(totalLoans * nplFrac),
    PLN(capital),
    PLN(deposits),
    PLN(bonds * (BigDecimal(1) - htmFrac)),
    PLN(bonds * htmFrac),
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
  )

  val genGovState: Gen[FiscalBudget.GovState] = for
    taxRev      <- genDecimal("0.0", "1000000000.0")
    deficit     <- genDecimal("-1000000000.0", "1000000000.0")
    cumDebt     <- genDecimal("0.0", "10000000000.0")
    unempBen    <- genDecimal("0.0", "100000000.0")
    bondYield   <- genDecimal("0.0", "0.15")
    debtService <- genDecimal("0.0", "100000000.0")
  yield FiscalBudget.GovState(
    PLN(taxRev),
    PLN(deficit),
    PLN(cumDebt),
    PLN(unempBen),
    Rate(bondYield),
    Rate(bondYield), // weightedCoupon starts at market yield
    PLN(debtService),
  )

  val genForexState: Gen[OpenEconomy.ForexState] = for
    er      <- genExchangeRate
    imports <- genDecimal("0.0", "1000000000.0")
    exports <- genDecimal("0.0", "1000000000.0")
    techImp <- genDecimal("0.0", "100000000.0")
  yield OpenEconomy.ForexState(ExchangeRate(er), PLN(imports), PLN(exports), PLN(exports - imports), PLN(techImp))

  val genBopState: Gen[OpenEconomy.BopState] = for
    nfa     <- genDecimal("-10000000000.0", "10000000000.0")
    fAssets <- genDecimal("0.0", "10000000000.0")
    fLiab   <- genDecimal("0.0", "10000000000.0")
    tb      <- genDecimal("-1000000000.0", "1000000000.0")
    pi      <- genDecimal("-100000000.0", "100000000.0")
    si      <- genDecimal("0.0", "10000000.0")
    fdi     <- genDecimal("0.0", "100000000.0")
    pf      <- genDecimal("-100000000.0", "100000000.0")
    res     <- genDecimal("0.0", "1000000000.0")
    exp     <- genDecimal("0.0", "1000000000.0")
    totImp  <- genDecimal("0.0", "1000000000.0")
    impInt  <- genDecimal("0.0", "100000000.0")
  yield
    val ca = tb + pi + si
    val ka = fdi + pf
    OpenEconomy.BopState(
      PLN(nfa),
      PLN(fAssets),
      PLN(fLiab),
      PLN(ca),
      PLN(ka),
      PLN(tb),
      PLN(pi),
      PLN(si),
      PLN(fdi),
      PLN(pf),
      PLN(res),
      PLN(exp),
      PLN(totImp),
      PLN(impInt),
    )

  // --- HhStatus generators ---

  val genHhStatus: Gen[HhStatus] = Gen.oneOf(
    for
      fid    <- Gen.choose(0, 9999)
      sector <- Gen.choose(0, 5)
      wage   <- genWage
    yield HhStatus.Employed(FirmId(fid), SectorIdx(sector), PLN(wage)),
    Gen.choose(0, 24).map(m => HhStatus.Unemployed(m)),
    for
      ml   <- Gen.choose(1, 6)
      sec  <- Gen.choose(0, 5)
      cost <- genDecimal("1000.0", "10000.0")
    yield HhStatus.Retraining(ml, SectorIdx(sec), PLN(cost)),
    Gen.const(HhStatus.Bankrupt),
  )

  // --- Household generators ---

  val genHousehold: Gen[Household.State] = for
    id      <- Gen.choose(0, 99999)
    savings <- genDecimal("-50000.0", "500000.0")
    debt    <- genDecimal("0.0", "200000.0")
    rent    <- genDecimal("800.0", "5000.0")
    skill   <- genDecimal("0.3", "1.0")
    health  <- genDecimal("0.0", "0.5")
    mpc     <- genDecimal("0.5", "0.98")
    status  <- genHhStatus
    bankId  <- Gen.choose(0, 6)
    eqW     <- genDecimal("0.0", "100000.0")
    lastSec <- Gen.choose(-1, 5)
  yield TestHouseholdState(
    HhId(id),
    PLN(savings),
    PLN(debt),
    PLN(rent),
    Share(skill),
    Share(health),
    Share(mpc),
    status,
    Array.empty[HhId],
    BankId(bankId),
    PLN(eqW),
    SectorIdx(lastSec),
    isImmigrant = false,
    numDependentChildren = 0,
    consumerDebt = PLN.Zero,
    education = 2,
    taskRoutineness = Share("0.5"),
    wageScar = Share.Zero,
  )

  // --- World generator ---

  val genWorld: Gen[World] = for
    infl     <- genInflation
    price    <- genPrice
    gov      <- genGovState
    rate     <- genRate
    forex    <- genForexState
    employed <- Gen.choose(0, p.pop.firmsCount * p.pop.workersPerFirm)
    wage     <- genWage
    resWage  <- genDecimal("4666.0", "10000.0")
    autoR    <- genFraction
    hybR     <- genFraction
    gdp      <- genDecimal("1000000.0", "100000000000.0")
  yield testWorld(
    inflation = Rate(infl),
    priceLevel = PriceIndex(price),
    monthlyGdpProxy = PLN(gdp),
    currentSigmas = p.sectorDefs.map(_.sigma),
    totalPopulation = employed,
    employed = employed,
    marketWage = PLN(wage),
    reservationWage = PLN(resWage),
    gov = gov,
    nbp = Nbp.State(Rate(rate), false, PLN.Zero, PLN.Zero),
    forex = forex,
    real = RealState.zero.copy(automationRatio = Share(autoR), hybridRatio = Share(hybR)),
  )

  // --- SFC Check generators ---

  val genSnapshot: Gen[Sfc.StockState] = for
    hhS       <- genDecimal("0.0", "10000000000.0")
    hhD       <- genDecimal("0.0", "1000000000.0")
    fCash     <- genDecimal("0.0", "10000000000.0")
    fDebt     <- genDecimal("0.0", "10000000000.0")
    bCap      <- genDecimal("0.0", "1000000000.0")
    bDep      <- genDecimal("0.0", "10000000000.0")
    bLoans    <- genDecimal("0.0", "10000000000.0")
    govDebt   <- genDecimal("0.0", "10000000000.0")
    nfa       <- genDecimal("-10000000000.0", "10000000000.0")
    bankBonds <- genDecimal("0.0", "10000000000.0")
    nbpBonds  <- genDecimal("0.0", "10000000000.0")
    jstDep    <- genDecimal("0.0", "1000000000.0")
    jstDebt   <- genDecimal("0.0", "1000000000.0")
    fusBal    <- genDecimal("-10000000000.0", "10000000000.0")
    ppkBonds  <- genDecimal("0.0", "1000000000.0")
    mortStock <- genDecimal("0.0", "1000000000000.0")
  yield
    val bankBondHoldings = PLN(bankBonds)
    val nbpBondHoldings  = PLN(nbpBonds)
    val ppkBondHoldings  = PLN(ppkBonds)
    Sfc.StockState(
      hhSavings = PLN(hhS),
      hhDebt = PLN(hhD),
      firmCash = PLN(fCash),
      firmDebt = PLN(fDebt),
      bankCapital = PLN(bCap),
      bankDeposits = PLN(bDep),
      bankLoans = PLN(bLoans),
      govDebt = PLN(govDebt),
      nfa = PLN(nfa),
      bankBondHoldings = bankBondHoldings,
      nbpBondHoldings = nbpBondHoldings,
      bondsOutstanding = bankBondHoldings + nbpBondHoldings + ppkBondHoldings,
      interbankNetSum = PLN.Zero,
      jstDeposits = PLN(jstDep),
      jstDebt = PLN(jstDebt),
      fusBalance = PLN(fusBal),
      nfzBalance = PLN.Zero,
      foreignBondHoldings = PLN.Zero,
      ppkBondHoldings = ppkBondHoldings,
      mortgageStock = PLN(mortStock),
      consumerLoans = PLN.Zero,
      corpBondsOutstanding = PLN.Zero,
      insuranceGovBondHoldings = PLN.Zero,
      tfiGovBondHoldings = PLN.Zero,
      nbfiLoanStock = PLN.Zero,
      quasiFiscalBondsOutstanding = PLN.Zero,
      quasiFiscalBankHoldings = PLN.Zero,
      quasiFiscalNbpHoldings = PLN.Zero,
      quasiFiscalLoanPortfolio = PLN.Zero,
    )

  val genMonthlyFlows: Gen[Sfc.SemanticFlows] = for
    govSpend     <- genDecimal("0.0", "1000000000.0")
    govRev       <- genDecimal("0.0", "1000000000.0")
    nplLoss      <- genDecimal("0.0", "100000000.0")
    intIncome    <- genDecimal("0.0", "100000000.0")
    hhDebtSvc    <- genDecimal("0.0", "10000000.0")
    totIncome    <- genDecimal("0.0", "10000000000.0")
    totCons      <- genDecimal("0.0", "10000000000.0")
    newLoans     <- genDecimal("0.0", "1000000000.0")
    nplRecov     <- genDecimal("0.0", "100000000.0")
    ca           <- genDecimal("-1000000000.0", "1000000000.0")
    valEff       <- genDecimal("-100000000.0", "100000000.0")
    bankBondInc  <- genDecimal("0.0", "100000000.0")
    qePurchase   <- genDecimal("0.0", "1000000000.0")
    newBondIssue <- genDecimal("0.0", "1000000000.0")
    depIntPaid   <- genDecimal("0.0", "10000000.0")
    resInt       <- genDecimal("0.0", "10000000.0")
    sfIncome     <- genDecimal("-1000000.0", "10000000.0")
    ibInterest   <- genDecimal("-10000000.0", "10000000.0")
    jstDepChg    <- genDecimal("-10000000.0", "10000000.0")
    jstSpend     <- genDecimal("0.0", "100000000.0")
    jstRev       <- genDecimal("0.0", "100000000.0")
    zusContrib   <- genDecimal("0.0", "1000000000.0")
    zusPension   <- genDecimal("0.0", "1000000000.0")
    zusGovSub    <- genDecimal("0.0", "100000000.0")
    divIncome    <- genDecimal("0.0", "100000000.0")
    foreignDiv   <- genDecimal("0.0", "100000000.0")
    divTax       <- genDecimal("0.0", "10000000.0")
    mortIntInc   <- genDecimal("0.0", "100000000.0")
    mortNplLoss  <- genDecimal("0.0", "10000000.0")
    mortOrig     <- genDecimal("0.0", "1000000000.0")
    mortPrinc    <- genDecimal("0.0", "100000000.0")
    mortDefAmt   <- genDecimal("0.0", "10000000.0")
  yield Sfc.SemanticFlows(
    govSpending = PLN(govSpend),
    govRevenue = PLN(govRev),
    nplLoss = PLN(nplLoss),
    interestIncome = PLN(intIncome),
    hhDebtService = PLN(hhDebtSvc),
    totalIncome = PLN(totIncome),
    totalConsumption = PLN(totCons),
    newLoans = PLN(newLoans),
    nplRecovery = PLN(nplRecov),
    currentAccount = PLN(ca),
    valuationEffect = PLN(valEff),
    bankBondIncome = PLN(bankBondInc),
    qePurchase = PLN(qePurchase),
    newBondIssuance = PLN(newBondIssue),
    depositInterestPaid = PLN(depIntPaid),
    reserveInterest = PLN(resInt),
    standingFacilityIncome = PLN(sfIncome),
    interbankInterest = PLN(ibInterest),
    jstDepositChange = PLN(jstDepChg),
    jstSpending = PLN(jstSpend),
    jstRevenue = PLN(jstRev),
    zusContributions = PLN(zusContrib),
    zusPensionPayments = PLN(zusPension),
    zusGovSubvention = PLN(zusGovSub),
    nfzContributions = PLN.Zero,
    nfzSpending = PLN.Zero,
    nfzGovSubvention = PLN.Zero,
    dividendIncome = PLN(divIncome),
    foreignDividendOutflow = PLN(foreignDiv),
    dividendTax = PLN(divTax),
    mortgageInterestIncome = PLN(mortIntInc),
    mortgageNplLoss = PLN(mortNplLoss),
    mortgageOrigination = PLN(mortOrig),
    mortgagePrincipalRepaid = PLN(mortPrinc),
    mortgageDefaultAmount = PLN(mortDefAmt),
    remittanceOutflow = PLN.Zero,
    fofResidual = PLN.Zero,
    consumerDebtService = PLN.Zero,
    consumerNplLoss = PLN.Zero,
    consumerOrigination = PLN.Zero,
    consumerPrincipalRepaid = PLN.Zero,
    consumerDefaultAmount = PLN.Zero,
    corpBondCouponIncome = PLN.Zero,
    corpBondDefaultLoss = PLN.Zero,
    corpBondIssuance = PLN.Zero,
    corpBondAmortization = PLN.Zero,
    corpBondDefaultAmount = PLN.Zero,
    insNetDepositChange = PLN.Zero,
    nbfiDepositDrain = PLN.Zero,
    nbfiOrigination = PLN.Zero,
    nbfiRepayment = PLN.Zero,
    nbfiDefaultAmount = PLN.Zero,
    fdiProfitShifting = PLN.Zero,
    fdiRepatriation = PLN.Zero,
    diasporaInflow = PLN.Zero,
    tourismExport = PLN.Zero,
    tourismImport = PLN.Zero,
    bfgLevy = PLN.Zero,
    bailInLoss = PLN.Zero,
    bankCapitalDestruction = PLN.Zero,
    investNetDepositFlow = PLN.Zero,
    firmPrincipalRepaid = PLN.Zero,
    unrealizedBondLoss = PLN.Zero,
    htmRealizedLoss = PLN.Zero,
    eclProvisionChange = PLN.Zero,
    quasiFiscalBondIssuance = PLN.Zero,
    quasiFiscalBondAmortization = PLN.Zero,
    quasiFiscalNbpBondAmortization = PLN.Zero,
    quasiFiscalNbpAbsorption = PLN.Zero,
    quasiFiscalLending = PLN.Zero,
    quasiFiscalRepayment = PLN.Zero,
    quasiFiscalDepositChange = PLN.Zero,
  )

  /** Generate (prev, curr, flows) where exact SFC identities hold. */
  val genConsistentFlowsAndSnapshots: Gen[(Sfc.StockState, Sfc.StockState, Sfc.SemanticFlows)] =
    for
      prev  <- genSnapshot
      flows <- genMonthlyFlows
    yield
      val expectedBankCapChange  = -flows.nplLoss - flows.mortgageNplLoss - flows.consumerNplLoss
        - flows.corpBondDefaultLoss - flows.bfgLevy - flows.unrealizedBondLoss - flows.htmRealizedLoss - flows.bankCapitalDestruction +
        (flows.interestIncome + flows.hhDebtService + flows.bankBondIncome
          + flows.mortgageInterestIncome + flows.consumerDebtService + flows.corpBondCouponIncome
          - flows.depositInterestPaid
          + flows.reserveInterest + flows.standingFacilityIncome + flows.interbankInterest) * Share("0.3")
      val expectedDepChange      = flows.totalIncome - flows.totalConsumption + flows.investNetDepositFlow +
        flows.jstDepositChange +
        flows.dividendIncome - flows.foreignDividendOutflow - flows.remittanceOutflow + flows.diasporaInflow +
        flows.tourismExport - flows.tourismImport - flows.bailInLoss +
        flows.newLoans - flows.firmPrincipalRepaid +
        flows.consumerOrigination + flows.insNetDepositChange + flows.nbfiDepositDrain +
        flows.quasiFiscalDepositChange
      val expectedGovDebtChange  = flows.govSpending - flows.govRevenue
      val expectedNfaChange      = flows.currentAccount + flows.valuationEffect
      val expectedJstDebtChange  = flows.jstSpending - flows.jstRevenue
      val expectedFusChange      = flows.zusContributions + flows.zusGovSubvention - flows.zusPensionPayments
      val expectedMortgageChange =
        flows.mortgageOrigination - flows.mortgagePrincipalRepaid - flows.mortgageDefaultAmount
      val expectedCcChange       =
        flows.consumerOrigination - flows.consumerPrincipalRepaid - flows.consumerDefaultAmount
      val expectedCorpBondChange =
        flows.corpBondIssuance - flows.corpBondAmortization - flows.corpBondDefaultAmount
      val expectedNbfiChange     =
        flows.nbfiOrigination - flows.nbfiRepayment - flows.nbfiDefaultAmount
      val curr                   = prev.copy(
        bankCapital = prev.bankCapital + expectedBankCapChange,
        bankDeposits = prev.bankDeposits + expectedDepChange,
        govDebt = prev.govDebt + expectedGovDebtChange,
        nfa = prev.nfa + expectedNfaChange,
        jstDebt = prev.jstDebt + expectedJstDebtChange,
        fusBalance = prev.fusBalance + expectedFusChange,
        mortgageStock = prev.mortgageStock + expectedMortgageChange,
        consumerLoans = prev.consumerLoans + expectedCcChange,
        corpBondsOutstanding = prev.corpBondsOutstanding + expectedCorpBondChange,
        nbfiLoanStock = prev.nbfiLoanStock + expectedNbfiChange,
      )
      // Bond clearing: bankBondHoldings + nbpBondHoldings + ppkBondHoldings = bondsOutstanding
      // genSnapshot already ensures this for prev; curr inherits prev's bond fields unchanged
      (prev, curr, flows)

  // --- Sorted array generator (for Gini tests) ---

  def genSortedArray(n: Int): Gen[Array[Long]] =
    Gen.listOfN(n, Gen.choose(0L, 100000L)).map(_.toArray.sorted)

  def genSortedArrayWithSize: Gen[Array[Long]] = for
    n   <- Gen.choose(2, 200)
    arr <- Gen.listOfN(n, Gen.choose(0L, 100000L))
  yield arr.toArray.sorted

  // --- Nbp.State generator ---

  val genNbpState: Gen[Nbp.State] = for
    rate     <- genRate
    qeActive <- Gen.oneOf(true, false)
    qeCum    <- genDecimal("0.0", "10000000000.0")
    lastFx   <- genDecimal("-1000000000.0", "1000000000.0")
  yield Nbp.State(Rate(rate), qeActive, PLN(qeCum), PLN(lastFx))

  // --- I-O matrix generator ---

  private def genIoColumn(cellsLeft: Int, remaining: Long): Gen[Vector[Long]] =
    if cellsLeft <= 1 then Gen.const(Vector(remaining))
    else
      for
        head <- Gen.chooseNum[Long](0L, remaining)
        tail <- genIoColumn(cellsLeft - 1, remaining - head)
      yield head +: tail

  private def shareSafe(raw: Long): Share =
    if raw <= 0L then Share.Zero
    else Share.fromRaw(raw - 1L)

  val genIoMatrix: Gen[Vector[Vector[Share]]] =
    val scale       = Share.One.toLong
    val sectorCount = p.io.matrix.length
    Gen
      .sequence[Vector[Vector[Long]], Vector[Long]](
        0.until(sectorCount)
          .map: _ =>
            for
              columnBudget <- Gen.chooseNum[Long](0L, scale - 1L)
              column       <- genIoColumn(sectorCount, columnBudget)
            yield column,
      )
      .map: columns =>
        Vector.tabulate(sectorCount, sectorCount): (i, j) =>
          shareSafe(columns(j)(i))

  // --- Banking sector generators ---

  object genBanking:
    case class SectorFixture(
        banks: Vector[Banking.BankState],
        financialStocks: Vector[Banking.BankFinancialStocks],
        configs: Vector[Banking.Config],
    )

    val Config: Gen[Banking.Config] = for
      id     <- Gen.choose(0, 6)
      share  <- genDecimal("0.01", "0.50")
      cet1   <- genDecimal("0.10", "0.25")
      spread <- genDecimal("-0.005", "0.005")
      aff    <- Gen.sequence[Vector[BigDecimal], BigDecimal]((0 until 6).map(_ => genDecimal("0.05", "0.40")))
    yield Banking.Config(BankId(id), s"Bank$id", Share(share), Share(cet1), Rate(spread), aff.map(Share(_)))

    val BankRow: Gen[(Banking.BankState, Banking.BankFinancialStocks)] = for
      id       <- Gen.choose(0, 6)
      deposits <- genDecimal("1000000.0", "10000000000.0")
      loans    <- genDecimal("0.0", "10000000000.0")
      capital  <- genDecimal("100000.0", "1000000000.0")
      nplFrac  <- genDecimal("0.0", "0.20")
      bonds    <- genDecimal("0.0", "1000000000.0")
      htmFrac  <- genFraction
      bookYld  <- genDecimal("0.0", "0.15")
      reserves <- genDecimal("0.0", "100000000.0")
      ibNet    <- genDecimal("-100000000.0", "100000000.0")
      failed   <- Gen.oneOf(false, false, false, false, true) // 20% chance
      lowCar   <- Gen.choose(0, 5)
    yield (
      Banking.BankState(
        id = BankId(id),
        capital = PLN(capital),
        nplAmount = PLN(loans * nplFrac),
        htmBookYield = Rate(bookYld),
        status = if failed then Banking.BankStatus.Failed(SimulationMonth.ExecutionMonth(30)) else Banking.BankStatus.Active(lowCar),
        loansShort = PLN.Zero,
        loansMedium = PLN.Zero,
        loansLong = PLN.Zero,
        consumerNpl = PLN.Zero,
      ),
      Banking.BankFinancialStocks(
        totalDeposits = PLN(deposits),
        firmLoan = PLN(loans),
        govBondAfs = PLN(bonds * (BigDecimal(1) - htmFrac)),
        govBondHtm = PLN(bonds * htmFrac),
        reserve = PLN(reserves),
        interbankLoan = PLN(ibNet),
        demandDeposit = PLN.Zero,
        termDeposit = PLN.Zero,
        consumerLoan = PLN.Zero,
      ),
    )

    val BankState: Gen[Banking.BankState] = BankRow.map(_._1)

    val Sector: Gen[SectorFixture] = for
      nBanks <- Gen.choose(2, 7)
      rows   <- Gen.listOfN(nBanks, BankRow).map(_.toVector.zipWithIndex.map { case ((bank, stocks), i) => (bank.copy(id = BankId(i)), stocks) })
      cfgs   <- Gen.listOfN(nBanks, Config).map(_.toVector.zipWithIndex.map((c, i) => c.copy(id = BankId(i))))
    yield SectorFixture(rows.map(_._1), rows.map(_._2), cfgs)
