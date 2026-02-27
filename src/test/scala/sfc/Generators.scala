package sfc.testutil

import org.scalacheck.{Arbitrary, Gen}
import _root_.sfc.agents.*
import _root_.sfc.sfc.*
import _root_.sfc.engine.World
import _root_.sfc.config.{Config, SECTORS, RunConfig, MonetaryRegime}

object Generators:

  // --- Primitive generators ---

  val genRate: Gen[Double] = Gen.choose(0.0, 0.25)

  val genPrice: Gen[Double] = Gen.choose(0.30, 5.0)

  val genExchangeRate: Gen[Double] = Gen.choose(2.5, 10.0)

  val genWage: Gen[Double] = Gen.choose(4666.0, 30000.0)

  val genInflation: Gen[Double] = Gen.choose(-0.50, 0.50)

  val genSigma: Gen[Double] = Gen.choose(0.1, 100.0)

  val genFraction: Gen[Double] = Gen.choose(0.0, 1.0)

  val genPositiveDouble: Gen[Double] = Gen.choose(1.0, 1e9)

  val genSmallPositiveDouble: Gen[Double] = Gen.choose(0.0, 1e7)

  // --- TechState generators ---

  val genTechState: Gen[TechState] = Gen.oneOf(
    Gen.choose(1, 20).map(w => TechState.Traditional(w)),
    for
      w   <- Gen.choose(1, 15)
      eff <- Gen.choose(0.5, 2.0)
    yield TechState.Hybrid(w, eff),
    Gen.choose(0.5, 3.0).map(e => TechState.Automated(e)),
    Gen.const(TechState.Bankrupt("test"))
  )

  val genAliveTechState: Gen[TechState] = Gen.oneOf(
    Gen.choose(1, 20).map(w => TechState.Traditional(w)),
    for
      w   <- Gen.choose(1, 15)
      eff <- Gen.choose(0.5, 2.0)
    yield TechState.Hybrid(w, eff),
    Gen.choose(0.5, 3.0).map(e => TechState.Automated(e))
  )

  // --- Firm generators ---

  val genFirm: Gen[Firm] = for
    id     <- Gen.choose(0, 9999)
    cash   <- Gen.choose(-100000.0, 5000000.0)
    debt   <- Gen.choose(0.0, 3000000.0)
    tech   <- genTechState
    risk   <- genFraction
    innov  <- Gen.choose(0.5, 2.0)
    digiR  <- Gen.choose(0.02, 0.98)
    sector <- Gen.choose(0, 5)
  yield Firm(id, cash, debt, tech, risk, innov, digiR, sector, Array.empty)

  val genAliveFirm: Gen[Firm] = for
    id     <- Gen.choose(0, 9999)
    cash   <- Gen.choose(0.0, 5000000.0)
    debt   <- Gen.choose(0.0, 3000000.0)
    tech   <- genAliveTechState
    risk   <- genFraction
    innov  <- Gen.choose(0.5, 2.0)
    digiR  <- Gen.choose(0.02, 0.98)
    sector <- Gen.choose(0, 5)
  yield Firm(id, cash, debt, tech, risk, innov, digiR, sector, Array.empty)

  // --- Balance sheet state generators ---

  val genBankState: Gen[BankState] = for
    totalLoans <- Gen.choose(1000.0, 1e10)
    nplFrac    <- Gen.choose(0.0, 0.30)
    capital    <- Gen.choose(1000.0, 1e9)
    deposits   <- Gen.choose(0.0, 1e10)
  yield BankState(totalLoans, totalLoans * nplFrac, capital, deposits)

  val genGovState: Gen[GovState] = for
    bdpActive <- Gen.oneOf(true, false)
    taxRev    <- Gen.choose(0.0, 1e9)
    bdpSpend  <- Gen.choose(0.0, 1e9)
    deficit   <- Gen.choose(-1e9, 1e9)
    cumDebt   <- Gen.choose(0.0, 1e10)
    unempBen  <- Gen.choose(0.0, 1e8)
  yield GovState(bdpActive, taxRev, bdpSpend, deficit, cumDebt, unempBen)

  val genForexState: Gen[ForexState] = for
    er       <- genExchangeRate
    imports  <- Gen.choose(0.0, 1e9)
    exports  <- Gen.choose(0.0, 1e9)
    techImp  <- Gen.choose(0.0, 1e8)
  yield ForexState(er, imports, exports, exports - imports, techImp)

  val genBopState: Gen[BopState] = for
    nfa     <- Gen.choose(-1e10, 1e10)
    fAssets <- Gen.choose(0.0, 1e10)
    fLiab   <- Gen.choose(0.0, 1e10)
    tb      <- Gen.choose(-1e9, 1e9)
    pi      <- Gen.choose(-1e8, 1e8)
    si      <- Gen.choose(0.0, 1e7)
    fdi     <- Gen.choose(0.0, 1e8)
    pf      <- Gen.choose(-1e8, 1e8)
    res     <- Gen.choose(0.0, 1e9)
    exp     <- Gen.choose(0.0, 1e9)
    totImp  <- Gen.choose(0.0, 1e9)
    impInt  <- Gen.choose(0.0, 1e8)
  yield
    val ca = tb + pi + si
    val ka = fdi + pf
    BopState(nfa, fAssets, fLiab, ca, ka, tb, pi, si, fdi, pf, res, exp, totImp, impInt)

  // --- HhStatus generators ---

  val genHhStatus: Gen[HhStatus] = Gen.oneOf(
    for
      fid    <- Gen.choose(0, 9999)
      sector <- Gen.choose(0, 5)
      wage   <- genWage
    yield HhStatus.Employed(fid, sector, wage),
    Gen.choose(0, 24).map(m => HhStatus.Unemployed(m)),
    for
      ml   <- Gen.choose(1, 6)
      sec  <- Gen.choose(0, 5)
      cost <- Gen.choose(1000.0, 10000.0)
    yield HhStatus.Retraining(ml, sec, cost),
    Gen.const(HhStatus.Bankrupt)
  )

  // --- Household generators ---

  val genHousehold: Gen[Household] = for
    id      <- Gen.choose(0, 99999)
    savings <- Gen.choose(-50000.0, 500000.0)
    debt    <- Gen.choose(0.0, 200000.0)
    rent    <- Gen.choose(800.0, 5000.0)
    skill   <- Gen.choose(0.3, 1.0)
    health  <- Gen.choose(0.0, 0.5)
    mpc     <- Gen.choose(0.5, 0.98)
    status  <- genHhStatus
  yield Household(id, savings, debt, rent, skill, health, mpc, status, Array.empty)

  // --- World generator ---

  val genWorld: Gen[World] = for
    month    <- Gen.choose(1, 120)
    infl     <- genInflation
    price    <- genPrice
    demand   <- Gen.choose(0.5, 2.0)
    gov      <- genGovState
    rate     <- genRate
    bank     <- genBankState
    forex    <- genForexState
    employed <- Gen.choose(0, Config.TotalPopulation)
    wage     <- genWage
    resWage  <- Gen.choose(4666.0, 10000.0)
    autoR    <- genFraction
    hybR     <- genFraction
    gdp      <- Gen.choose(1e6, 1e11)
  yield World(
    month, infl, price, demand, gov, NbpState(rate), bank, forex,
    HhState(employed, wage, resWage, 0.0, 0.0, 0.0, 0.0),
    autoR, hybR, gdp,
    SECTORS.map(_.sigma)
  )

  // --- SFC Check generators ---

  val genSnapshot: Gen[SfcCheck.Snapshot] = for
    hhS     <- Gen.choose(0.0, 1e10)
    hhD     <- Gen.choose(0.0, 1e9)
    fCash   <- Gen.choose(0.0, 1e10)
    fDebt   <- Gen.choose(0.0, 1e10)
    bCap    <- Gen.choose(0.0, 1e9)
    bDep    <- Gen.choose(0.0, 1e10)
    bLoans  <- Gen.choose(0.0, 1e10)
    govDebt <- Gen.choose(0.0, 1e10)
    nfa     <- Gen.choose(-1e10, 1e10)
  yield SfcCheck.Snapshot(hhS, hhD, fCash, fDebt, bCap, bDep, bLoans, govDebt, nfa)

  val genMonthlyFlows: Gen[SfcCheck.MonthlyFlows] = for
    govSpend  <- Gen.choose(0.0, 1e9)
    govRev    <- Gen.choose(0.0, 1e9)
    nplLoss   <- Gen.choose(0.0, 1e8)
    intIncome <- Gen.choose(0.0, 1e8)
    hhDebtSvc <- Gen.choose(0.0, 1e7)
    totIncome <- Gen.choose(0.0, 1e10)
    totCons   <- Gen.choose(0.0, 1e10)
    newLoans  <- Gen.choose(0.0, 1e9)
    nplRecov  <- Gen.choose(0.0, 1e8)
    ca        <- Gen.choose(-1e9, 1e9)
    valEff    <- Gen.choose(-1e8, 1e8)
  yield SfcCheck.MonthlyFlows(govSpend, govRev, nplLoss, intIncome, hhDebtSvc,
    totIncome, totCons, newLoans, nplRecov, ca, valEff)

  /** Generate (prev, curr, flows) where all 4 SFC identities hold exactly. */
  val genConsistentFlowsAndSnapshots: Gen[(SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)] =
    for
      prev  <- genSnapshot
      flows <- genMonthlyFlows
    yield
      val expectedBankCapChange = -flows.nplLoss + flows.interestIncome * 0.3 + flows.hhDebtService * 0.3
      val expectedDepChange = flows.totalIncome - flows.totalConsumption
      val expectedGovDebtChange = flows.govSpending - flows.govRevenue
      val expectedNfaChange = flows.currentAccount + flows.valuationEffect
      val curr = prev.copy(
        bankCapital = prev.bankCapital + expectedBankCapChange,
        bankDeposits = prev.bankDeposits + expectedDepChange,
        govDebt = prev.govDebt + expectedGovDebtChange,
        nfa = prev.nfa + expectedNfaChange
      )
      (prev, curr, flows)

  // --- RunConfig generators ---

  val genRunConfig: Gen[RunConfig] = for
    bdp    <- Gen.choose(0.0, 5000.0)
    regime <- Gen.oneOf(MonetaryRegime.Pln, MonetaryRegime.Eur)
  yield RunConfig(bdp, 1, "test", regime)

  // --- Sorted array generator (for Gini tests) ---

  def genSortedArray(n: Int): Gen[Array[Double]] =
    Gen.listOfN(n, Gen.choose(0.0, 100000.0)).map(_.toArray.sorted)

  def genSortedArrayWithSize: Gen[Array[Double]] = for
    n   <- Gen.choose(2, 200)
    arr <- Gen.listOfN(n, Gen.choose(0.0, 100000.0))
  yield arr.toArray.sorted

  // --- I-O matrix generator ---

  val genIoMatrix: Gen[Vector[Vector[Double]]] =
    Gen.sequence[Vector[Vector[Double]], Vector[Double]](
      (0.until(6)).map { _ =>
        Gen.sequence[Vector[Double], Double](
          (0.until(6)).map(_ => Gen.choose(0.0, 0.15))
        )
      }
    ).suchThat { m =>
      // column sums must be < 1.0
      (0.until(6)).forall(j => m.map(_(j)).sum < 1.0)
    }
