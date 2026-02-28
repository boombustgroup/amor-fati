package sfc.engine

import sfc.config.Config
import sfc.sfc.BankState

import scala.util.Random

/** Configuration for a single bank in the multi-bank system. */
case class BankConfig(
  id: Int,
  name: String,
  initMarketShare: Double,
  initCet1: Double,
  lendingSpread: Double,
  sectorAffinity: Vector[Double]
)

/** State of an individual bank (updated each month). */
case class IndividualBankState(
  id: Int,
  deposits: Double,
  loans: Double,
  capital: Double,
  nplAmount: Double,
  govBondHoldings: Double,
  reservesAtNbp: Double,
  interbankNet: Double,
  failed: Boolean,
  failedMonth: Int,
  consecutiveLowCar: Int,
  // Maturity mismatch
  demandDeposits: Double = 0.0,    // demand deposits (% split from deposits)
  termDeposits: Double = 0.0,      // term deposits
  loansShort: Double = 0.0,        // short-term loans (< 1 year)
  loansMedium: Double = 0.0,       // medium-term (1-5 years)
  loansLong: Double = 0.0          // long-term (> 5 years)
):
  def nplRatio: Double = if loans > 1.0 then nplAmount / loans else 0.0
  def car: Double = if loans > 1.0 then capital / loans else 10.0

  /** High Quality Liquid Assets: reserves + gov bonds (Level 1 assets). */
  def hqla: Double = reservesAtNbp + govBondHoldings

  /** Net cash outflows (30-day): demand deposits × runoff rate. */
  def netCashOutflows: Double = demandDeposits * Config.BankDemandDepositRunoff

  /** LCR = HQLA / net cash outflows over 30 days. */
  def lcr: Double = if netCashOutflows > 1.0 then hqla / netCashOutflows else 10.0

  /** Available Stable Funding: capital + term deposits × 0.95 + demand deposits × 0.90. */
  def asf: Double = capital + termDeposits * 0.95 + demandDeposits * 0.90

  /** Required Stable Funding: short × 0.50 + medium × 0.65 + long × 0.85 + bonds × 0.05. */
  def rsf: Double = loansShort * 0.50 + loansMedium * 0.65 + loansLong * 0.85 + govBondHoldings * 0.05

  /** NSFR = ASF / RSF. */
  def nsfr: Double = if rsf > 1.0 then asf / rsf else 10.0

/** State of the entire banking sector. */
case class BankingSectorState(
  banks: Vector[IndividualBankState],
  interbankRate: Double,
  configs: Vector[BankConfig],
  interbankCurve: Option[InterbankCurve] = None
):
  def aggregate: BankState =
    BankState(
      totalLoans = banks.map(_.loans).sum,
      nplAmount = banks.map(_.nplAmount).sum,
      capital = banks.map(_.capital).sum,
      deposits = banks.map(_.deposits).sum,
      govBondHoldings = banks.map(_.govBondHoldings).sum
    )

object BankingSector:

  // 7 Polish banks (KNF 2024)
  val DefaultConfigs: Vector[BankConfig] = Vector(
    BankConfig(0, "PKO BP",     0.175, 0.185, -0.002, Vector(0.15, 0.15, 0.15, 0.10, 0.30, 0.15)),
    BankConfig(1, "Pekao",      0.120, 0.178, -0.001, Vector(0.15, 0.20, 0.20, 0.15, 0.15, 0.15)),
    BankConfig(2, "mBank",      0.085, 0.169,  0.000, Vector(0.30, 0.10, 0.25, 0.10, 0.10, 0.15)),
    BankConfig(3, "ING BSK",    0.075, 0.172, -0.001, Vector(0.15, 0.35, 0.15, 0.10, 0.10, 0.15)),
    BankConfig(4, "Santander",  0.070, 0.170,  0.000, Vector(0.15, 0.10, 0.35, 0.15, 0.10, 0.15)),
    BankConfig(5, "BPS/Coop",   0.050, 0.150,  0.003, Vector(0.05, 0.10, 0.10, 0.05, 0.05, 0.65)),
    BankConfig(6, "Others",     0.425, 0.165,  0.001, Vector(0.15, 0.17, 0.17, 0.17, 0.17, 0.17))
  )

  /** Initialize banking sector from total deposits and capital. */
  def initialize(totalDeposits: Double, totalCapital: Double,
                 configs: Vector[BankConfig]): BankingSectorState =
    val banks = configs.map { cfg =>
      IndividualBankState(
        id = cfg.id,
        deposits = totalDeposits * cfg.initMarketShare,
        loans = 0.0,
        capital = totalCapital * cfg.initMarketShare,
        nplAmount = 0.0,
        govBondHoldings = 0.0,
        reservesAtNbp = 0.0,
        interbankNet = 0.0,
        failed = false,
        failedMonth = 0,
        consecutiveLowCar = 0
      )
    }
    BankingSectorState(banks, 0.0, configs)

  /** Assign a firm to a bank based on sector affinity and market share. */
  def assignBank(firmSector: Int, configs: Vector[BankConfig], rng: Random): Int =
    val weights = configs.map(c => c.sectorAffinity(firmSector) * c.initMarketShare)
    val total = weights.sum
    if total <= 0.0 then 0
    else
      val r = rng.nextDouble() * total
      var cumulative = 0.0
      var i = 0
      while i < weights.length - 1 do
        cumulative += weights(i)
        if r < cumulative then return i
        i += 1
      weights.length - 1

  /** HH deposit rate (annual). Polish banks: NBP rate - spread. */
  def hhDepositRate(refRate: Double): Double =
    Math.max(0.0, refRate - Config.HhDepositSpread)

  /** Compute lending rate for a specific bank. */
  def lendingRate(bank: IndividualBankState, cfg: BankConfig, refRate: Double): Double =
    if bank.failed then refRate + 0.50
    else
      val nplSpread = Math.min(0.15, bank.nplRatio * Config.NplSpreadFactor)
      val carPenalty = if bank.car < Config.MinCar * 1.5 then
        Math.max(0.0, (Config.MinCar * 1.5 - bank.car) * 2.0)
      else 0.0
      refRate + Config.BaseSpread + cfg.lendingSpread + nplSpread + carPenalty

  /** Check if a bank can lend a given amount. */
  def canLend(bank: IndividualBankState, amount: Double, rng: Random,
              ccyb: Double = 0.0): Boolean =
    if bank.failed then false
    else
      val projectedCar = bank.capital / (bank.loans + amount)
      val approvalP = Math.max(0.1, 1.0 - bank.nplRatio * 3.0)
      // Use effective MinCAR (base + CCyB + OSII) when macropru enabled
      val minCar = Macroprudential.effectiveMinCar(bank.id, ccyb)
      val carOk = projectedCar >= minCar
      // LCR/NSFR constraints (only when enabled)
      val lcrOk = if Config.BankLcrEnabled then bank.lcr >= Config.BankLcrMin else true
      val nsfrOk = if Config.BankLcrEnabled then bank.nsfr >= Config.BankNsfrMin else true
      carOk && lcrOk && nsfrOk && rng.nextDouble() < approvalP

  /** Compute interbank rate (WIBOR proxy).
    * stress = 0 → deposit rate; stress = 1 → lombard rate. */
  def interbankRate(banks: Vector[IndividualBankState], refRate: Double): Double =
    val aggNpl = banks.filterNot(_.failed).map(_.nplAmount).sum
    val aggLoans = banks.filterNot(_.failed).map(_.loans).sum
    val aggNplRate = if aggLoans > 1.0 then aggNpl / aggLoans else 0.0
    val stress = Math.max(0.0, Math.min(1.0, aggNplRate / Config.BankStressThreshold))
    val depositRate = Math.max(0.0, refRate - 0.01)
    val lombardRate = refRate + 0.01
    depositRate + stress * (lombardRate - depositRate)

  /** Clear the interbank market: excess reserves → lender/borrower netting. */
  def clearInterbank(banks: Vector[IndividualBankState], configs: Vector[BankConfig],
                     rate: Double): Vector[IndividualBankState] =
    // Compute excess reserves for each bank
    val excess = banks.zip(configs).map { (b, _) =>
      if b.failed then 0.0
      else b.deposits * (1.0 - Config.BankReserveReq) - b.loans - b.govBondHoldings
    }
    val totalLending = excess.filter(_ > 0).sum
    val totalBorrowing = -excess.filter(_ < 0).sum

    if totalLending <= 0 || totalBorrowing <= 0 then
      // No interbank activity: zero out net positions
      banks.map(_.copy(interbankNet = 0.0, reservesAtNbp = 0.0))
    else
      // Credit rationing: if borrowing demand > lending supply, scale down
      val scale = Math.min(1.0, totalLending / totalBorrowing)
      banks.zip(excess).map { (b, ex) =>
        if b.failed then b.copy(interbankNet = 0.0, reservesAtNbp = 0.0)
        else if ex > 0 then
          // Net lender: lend proportional share
          val lent = ex * Math.min(1.0, totalBorrowing / totalLending)
          b.copy(interbankNet = lent, reservesAtNbp = ex - lent)
        else if ex < 0 then
          // Net borrower: borrow scaled amount
          val borrowed = -ex * scale
          b.copy(interbankNet = -borrowed, reservesAtNbp = 0.0)
        else
          b.copy(interbankNet = 0.0, reservesAtNbp = 0.0)
      }

  /** Check for bank failures (CAR < effectiveMinCar for 3 consecutive months, or LCR/NSFR breach). */
  def checkFailures(banks: Vector[IndividualBankState], month: Int,
                    enabled: Boolean = Config.BankFailureEnabled,
                    ccyb: Double = 0.0): (Vector[IndividualBankState], Boolean) =
    if !enabled then (banks.map(b => b.copy(consecutiveLowCar = 0)), false)
    else
      var anyFailed = false
      val updated = banks.map { b =>
        if b.failed then b
        else
          // Use effective MinCAR (base + CCyB + OSII) for failure threshold
          val minCar = Macroprudential.effectiveMinCar(b.id, ccyb)
          val lowCar = b.car < minCar
          // LCR/NSFR breach triggers immediate failure (severe liquidity crisis)
          val lcrBreach = Config.BankLcrEnabled && b.lcr < Config.BankLcrMin * 0.5  // below 50% of min
          val newConsec = if lowCar then b.consecutiveLowCar + 1 else 0
          if newConsec >= 3 || lcrBreach then
            anyFailed = true
            b.copy(failed = true, failedMonth = month, consecutiveLowCar = newConsec,
              capital = 0.0)  // Shareholders wiped
          else
            b.copy(consecutiveLowCar = newConsec)
      }
      (updated, anyFailed)

  /** BFG resolution: transfer deposits, bonds, performing loans to healthiest bank. */
  def resolveFailures(banks: Vector[IndividualBankState]): Vector[IndividualBankState] =
    val newlyFailed = banks.filter(b => b.failed && b.deposits > 0)
    if newlyFailed.isEmpty then banks
    else
      val absorberId = healthiestBankId(banks)
      banks.map { b =>
        if b.failed && b.deposits > 0 then
          // Wipe this bank's balance sheet
          b.copy(deposits = 0.0, loans = 0.0, govBondHoldings = 0.0,
            nplAmount = 0.0, interbankNet = 0.0, reservesAtNbp = 0.0)
        else if b.id == absorberId then
          // Absorb deposits, performing loans, bonds from failed banks
          val addedDeposits = newlyFailed.map(_.deposits).sum
          val addedLoans = newlyFailed.map(f => f.loans - f.nplAmount).sum
          val addedBonds = newlyFailed.map(_.govBondHoldings).sum
          b.copy(
            deposits = b.deposits + addedDeposits,
            loans = b.loans + Math.max(0, addedLoans),
            govBondHoldings = b.govBondHoldings + addedBonds
          )
        else b
      }

  /** Find the healthiest (highest CAR) surviving bank. */
  def healthiestBankId(banks: Vector[IndividualBankState]): Int =
    val alive = banks.filterNot(_.failed)
    if alive.isEmpty then 0
    else alive.maxBy(_.car).id

  /** Reassign a firm/household from a failed bank to the healthiest surviving bank. */
  def reassignBankId(currentBankId: Int, banks: Vector[IndividualBankState]): Int =
    if currentBankId < banks.length && !banks(currentBankId).failed then currentBankId
    else healthiestBankId(banks)

  /** Allocate new bond issuance to banks proportional to their deposits.
    * Falls back to equal shares when total deposits are non-positive
    * (can happen when large foreign dividend outflows drain the system). */
  def allocateBonds(banks: Vector[IndividualBankState], deficit: Double): Vector[IndividualBankState] =
    if deficit == 0.0 then return banks
    val aliveBanks = banks.filterNot(_.failed)
    val nAlive = aliveBanks.length
    if nAlive == 0 then return banks
    val totalDep = aliveBanks.map(_.deposits).sum
    banks.map { b =>
      if b.failed then b
      else
        val share = if totalDep > 0 then b.deposits / totalDep else 1.0 / nAlive
        b.copy(govBondHoldings = b.govBondHoldings + deficit * share)
    }

  /** Compute reserve interest for a single bank (monthly).
    * NBP pays NbpReserveRateMult × refRate (annual) on reserves. */
  def reserveInterest(bank: IndividualBankState, refRate: Double): Double =
    if bank.failed || bank.reservesAtNbp <= 0 then 0.0
    else bank.reservesAtNbp * refRate * Config.NbpReserveRateMult / 12.0

  /** Compute reserve interest for all banks. Returns per-bank vector + total. */
  def computeReserveInterest(banks: Vector[IndividualBankState], refRate: Double): (Vector[Double], Double) =
    val perBank = banks.map(b => reserveInterest(b, refRate))
    (perBank, perBank.sum)

  /** Compute standing facility flows for all banks (monthly).
    * Excess after interbank → deposit facility income (refRate − spread).
    * Shortfall after interbank → lombard cost (refRate + spread).
    * Returns per-bank net income vector + total net. */
  def computeStandingFacilities(banks: Vector[IndividualBankState],
                                refRate: Double): (Vector[Double], Double) =
    if !Config.NbpStandingFacilities then (banks.map(_ => 0.0), 0.0)
    else
      val depositRate = Math.max(0.0, refRate - Config.NbpDepositFacilitySpread)
      val lombardRate = refRate + Config.NbpLombardSpread
      val perBank = banks.map { b =>
        if b.failed then 0.0
        else if b.reservesAtNbp > 0 then
          // Parked excess at deposit facility → earn deposit rate
          b.reservesAtNbp * depositRate / 12.0
        else if b.interbankNet < 0 then
          // Still short after interbank → borrowing from lombard
          // The shortfall is already captured in interbankNet (negative = borrower)
          // Only charge lombard on the unfunded portion (reserves already 0)
          0.0  // credit rationing means the shortfall was already scaled in clearInterbank
        else 0.0
      }
      (perBank, perBank.sum)

  /** Compute interbank interest flows for all banks (monthly).
    * Lender earns interbankRate × lent / 12, borrower pays same.
    * Net zero in aggregate. */
  def interbankInterestFlows(banks: Vector[IndividualBankState], rate: Double): (Vector[Double], Double) =
    val perBank = banks.map { b =>
      if b.failed then 0.0
      else b.interbankNet * rate / 12.0  // positive for lenders, negative for borrowers
    }
    (perBank, perBank.sum)

  /** Allocate QE purchases from banks proportional to their bond holdings. */
  def allocateQePurchases(banks: Vector[IndividualBankState],
                          qeTotal: Double): Vector[IndividualBankState] =
    if qeTotal <= 0 then banks
    else
      val totalBonds = banks.filterNot(_.failed).map(_.govBondHoldings).sum
      if totalBonds <= 0 then banks
      else
        banks.map { b =>
          if b.failed || b.govBondHoldings <= 0 then b
          else
            val share = b.govBondHoldings / totalBonds
            val sold = Math.min(b.govBondHoldings, qeTotal * share)
            b.copy(govBondHoldings = b.govBondHoldings - sold)
        }
