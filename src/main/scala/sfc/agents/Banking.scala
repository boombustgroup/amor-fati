package sfc.agents

import sfc.engine.*
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object Banking:

  /** Configuration for a single bank in the multi-bank system. */
  case class Config(
    id: BankId,
    name: String,
    initMarketShare: Ratio,
    initCet1: Ratio,
    lendingSpread: Rate,
    sectorAffinity: Vector[Double],
  )

  /** State of an individual bank (updated each month). */
  case class BankState(
    id: BankId,
    deposits: PLN,
    loans: PLN,
    capital: PLN,
    nplAmount: PLN,
    govBondHoldings: PLN,
    reservesAtNbp: PLN,
    interbankNet: PLN,
    failed: Boolean,
    failedMonth: Int,
    consecutiveLowCar: Int,
    // Maturity mismatch
    demandDeposits: PLN = PLN.Zero, // demand deposits (% split from deposits)
    termDeposits: PLN = PLN.Zero, // term deposits
    loansShort: PLN = PLN.Zero, // short-term loans (< 1 year)
    loansMedium: PLN = PLN.Zero, // medium-term (1-5 years)
    loansLong: PLN = PLN.Zero, // long-term (> 5 years)
    consumerLoans: PLN = PLN.Zero, // consumer credit: outstanding unsecured HH loans
    consumerNpl: PLN = PLN.Zero, // consumer credit: NPL stock
    corpBondHoldings: PLN = PLN.Zero, // corporate bond holdings (bank share)
  ):
    def nplRatio: Double = if loans.toDouble > 1.0 then nplAmount / loans else 0.0
    def car: Double =
      val totalRwa = loans + consumerLoans + corpBondHoldings * 0.50 // 50% RW for corp bonds (Basel III BBB)
      if totalRwa.toDouble > 1.0 then capital / totalRwa else 10.0

    /** High Quality Liquid Assets: reserves + gov bonds (Level 1 assets). */
    def hqla: Double = (reservesAtNbp + govBondHoldings).toDouble

    /** Net cash outflows (30-day): demand deposits × runoff rate. */
    def netCashOutflows: Double = (demandDeposits * sfc.config.Config.BankDemandDepositRunoff).toDouble

    /** LCR = HQLA / net cash outflows over 30 days. */
    def lcr: Double = if netCashOutflows > 1.0 then hqla / netCashOutflows else 10.0

    /** Available Stable Funding: capital + term deposits × 0.95 + demand deposits × 0.90. */
    def asf: Double = (capital + termDeposits * 0.95 + demandDeposits * 0.90).toDouble

    /** Required Stable Funding: short × 0.50 + medium × 0.65 + long × 0.85 + gov bonds × 0.05 + corp bonds × 0.50. */
    def rsf: Double =
      (loansShort * 0.50 + loansMedium * 0.65 + loansLong * 0.85 + govBondHoldings * 0.05 + corpBondHoldings * 0.50).toDouble

    /** NSFR = ASF / RSF. */
    def nsfr: Double = if rsf > 1.0 then asf / rsf else 10.0

  /** State of the entire banking sector. */
  case class State(
    banks: Vector[BankState],
    interbankRate: Rate,
    configs: Vector[Config],
    interbankCurve: Option[YieldCurve.State] = None,
  ):
    def aggregate: sfc.accounting.BankingAggregate =
      sfc.accounting.BankingAggregate(
        totalLoans = PLN(banks.kahanSumBy(_.loans.toDouble)),
        nplAmount = PLN(banks.kahanSumBy(_.nplAmount.toDouble)),
        capital = PLN(banks.kahanSumBy(_.capital.toDouble)),
        deposits = PLN(banks.kahanSumBy(_.deposits.toDouble)),
        govBondHoldings = PLN(banks.kahanSumBy(_.govBondHoldings.toDouble)),
        consumerLoans = PLN(banks.kahanSumBy(_.consumerLoans.toDouble)),
        consumerNpl = PLN(banks.kahanSumBy(_.consumerNpl.toDouble)),
        corpBondHoldings = PLN(banks.kahanSumBy(_.corpBondHoldings.toDouble)),
      )

  // 7 Polish banks (KNF 2024)
  val DefaultConfigs: Vector[Config] = Vector(
    Config(BankId(0), "PKO BP", Ratio(0.175), Ratio(0.185), Rate(-0.002), Vector(0.15, 0.15, 0.15, 0.10, 0.30, 0.15)),
    Config(BankId(1), "Pekao", Ratio(0.120), Ratio(0.178), Rate(-0.001), Vector(0.15, 0.20, 0.20, 0.15, 0.15, 0.15)),
    Config(BankId(2), "mBank", Ratio(0.085), Ratio(0.169), Rate(0.000), Vector(0.30, 0.10, 0.25, 0.10, 0.10, 0.15)),
    Config(BankId(3), "ING BSK", Ratio(0.075), Ratio(0.172), Rate(-0.001), Vector(0.15, 0.35, 0.15, 0.10, 0.10, 0.15)),
    Config(BankId(4), "Santander", Ratio(0.070), Ratio(0.170), Rate(0.000), Vector(0.15, 0.10, 0.35, 0.15, 0.10, 0.15)),
    Config(BankId(5), "BPS/Coop", Ratio(0.050), Ratio(0.150), Rate(0.003), Vector(0.05, 0.10, 0.10, 0.05, 0.05, 0.65)),
    Config(BankId(6), "Others", Ratio(0.425), Ratio(0.165), Rate(0.001), Vector(0.15, 0.17, 0.17, 0.17, 0.17, 0.17)),
  )

  /** Initialize banking sector from total deposits, capital, loans, bonds, consumer loans. */
  def initialize(
    totalDeposits: Double,
    totalCapital: Double,
    totalLoans: Double = 0.0,
    totalGovBonds: Double = 0.0,
    totalConsumerLoans: Double = 0.0,
    configs: Vector[Config],
  ): State =
    val banks = configs.map { cfg =>
      BankState(
        id = cfg.id,
        deposits = PLN(totalDeposits * cfg.initMarketShare.toDouble),
        loans = PLN(totalLoans * cfg.initMarketShare.toDouble),
        capital = PLN(totalCapital * cfg.initMarketShare.toDouble),
        nplAmount = PLN.Zero,
        govBondHoldings = PLN(totalGovBonds * cfg.initMarketShare.toDouble),
        reservesAtNbp = PLN.Zero,
        interbankNet = PLN.Zero,
        failed = false,
        failedMonth = 0,
        consecutiveLowCar = 0,
        consumerLoans = PLN(totalConsumerLoans * cfg.initMarketShare.toDouble),
      )
    }
    State(banks, Rate.Zero, configs)

  /** Assign a firm to a bank based on sector affinity and market share. */
  def assignBank(firmSector: SectorIdx, configs: Vector[Config], rng: Random): BankId =
    val weights = configs.map(c => c.sectorAffinity(firmSector.toInt) * c.initMarketShare.toDouble)
    val total = weights.kahanSum
    if total <= 0.0 then BankId(0)
    else
      val r = rng.nextDouble() * total
      var cumulative = 0.0
      var i = 0
      while i < weights.length - 1 do
        cumulative += weights(i)
        if r < cumulative then return BankId(i)
        i += 1
      BankId(weights.length - 1)

  /** HH deposit rate (annual). Polish banks: NBP rate - spread. */
  def hhDepositRate(refRate: Double): Double =
    Math.max(0.0, refRate - sfc.config.Config.HhDepositSpread)

  /** Compute lending rate for a specific bank. */
  def lendingRate(bank: BankState, cfg: Config, refRate: Double): Double =
    if bank.failed then refRate + 0.50
    else
      val nplSpread = Math.min(0.15, bank.nplRatio * sfc.config.Config.NplSpreadFactor)
      val carPenalty =
        if bank.car < sfc.config.Config.MinCar * 1.5 then
          Math.max(0.0, (sfc.config.Config.MinCar * 1.5 - bank.car) * 2.0)
        else 0.0
      refRate + sfc.config.Config.BaseSpread + cfg.lendingSpread.toDouble + nplSpread + carPenalty

  /** Check if a bank can lend a given amount. */
  def canLend(bank: BankState, amount: Double, rng: Random, ccyb: Double = 0.0): Boolean =
    if bank.failed then false
    else
      val projectedCar =
        bank.capital.toDouble / (bank.loans.toDouble + bank.consumerLoans.toDouble + bank.corpBondHoldings.toDouble * 0.50 + amount)
      val approvalP = Math.max(0.1, 1.0 - bank.nplRatio * 3.0)
      val minCar = Macroprudential.effectiveMinCar(bank.id.toInt, ccyb)
      val carOk = projectedCar >= minCar
      val lcrOk = if sfc.config.Config.BankLcrEnabled then bank.lcr >= sfc.config.Config.BankLcrMin else true
      val nsfrOk = if sfc.config.Config.BankLcrEnabled then bank.nsfr >= sfc.config.Config.BankNsfrMin else true
      carOk && lcrOk && nsfrOk && rng.nextDouble() < approvalP

  /** Compute interbank rate (WIBOR proxy). stress = 0 → deposit rate; stress = 1 → lombard rate.
    */
  def interbankRate(banks: Vector[BankState], refRate: Double): Double =
    val aggNpl = banks.filterNot(_.failed).kahanSumBy(_.nplAmount.toDouble)
    val aggLoans = banks.filterNot(_.failed).kahanSumBy(_.loans.toDouble)
    val aggNplRate = if aggLoans > 1.0 then aggNpl / aggLoans else 0.0
    val stress = Math.max(0.0, Math.min(1.0, aggNplRate / sfc.config.Config.BankStressThreshold))
    val depositRate = Math.max(0.0, refRate - 0.01)
    val lombardRate = refRate + 0.01
    depositRate + stress * (lombardRate - depositRate)

  /** Clear the interbank market: excess reserves → lender/borrower netting. */
  def clearInterbank(banks: Vector[BankState], configs: Vector[Config], rate: Double): Vector[BankState] =
    val excess = banks.zip(configs).map { (b, _) =>
      if b.failed then 0.0
      else (b.deposits * (1.0 - sfc.config.Config.BankReserveReq) - b.loans - b.govBondHoldings).toDouble
    }
    val totalLending = excess.filter(_ > 0).kahanSum
    val totalBorrowing = -excess.filter(_ < 0).kahanSum

    if totalLending <= 0 || totalBorrowing <= 0 then
      banks.map(_.copy(interbankNet = PLN.Zero, reservesAtNbp = PLN.Zero))
    else
      val scale = Math.min(1.0, totalLending / totalBorrowing)
      banks.zip(excess).map { (b, ex) =>
        if b.failed then b.copy(interbankNet = PLN.Zero, reservesAtNbp = PLN.Zero)
        else if ex > 0 then
          val lent = ex * Math.min(1.0, totalBorrowing / totalLending)
          b.copy(interbankNet = PLN(lent), reservesAtNbp = PLN(ex - lent))
        else if ex < 0 then
          val borrowed = -ex * scale
          b.copy(interbankNet = PLN(-borrowed), reservesAtNbp = PLN.Zero)
        else b.copy(interbankNet = PLN.Zero, reservesAtNbp = PLN.Zero)
      }

  /** Check for bank failures (CAR < effectiveMinCar for 3 consecutive months, or LCR/NSFR breach). */
  def checkFailures(
    banks: Vector[BankState],
    month: Int,
    enabled: Boolean = sfc.config.Config.BankFailureEnabled,
    ccyb: Double = 0.0,
  ): (Vector[BankState], Boolean) =
    if !enabled then (banks.map(b => b.copy(consecutiveLowCar = 0)), false)
    else
      var anyFailed = false
      val updated = banks.map { b =>
        if b.failed then b
        else
          val minCar = Macroprudential.effectiveMinCar(b.id.toInt, ccyb)
          val lowCar = b.car < minCar
          val lcrBreach = sfc.config.Config.BankLcrEnabled && b.lcr < sfc.config.Config.BankLcrMin * 0.5
          val newConsec = if lowCar then b.consecutiveLowCar + 1 else 0
          if newConsec >= 3 || lcrBreach then
            anyFailed = true
            b.copy(failed = true, failedMonth = month, consecutiveLowCar = newConsec, capital = PLN.Zero)
          else b.copy(consecutiveLowCar = newConsec)
      }
      (updated, anyFailed)

  /** Compute monthly BFG levy for all banks. Returns per-bank vector + total. */
  def computeBfgLevy(banks: Vector[BankState]): (Vector[Double], Double) =
    val perBank = banks.map { b =>
      if b.failed then 0.0
      else (b.deposits * sfc.config.Config.BfgLevyRate / 12.0).toDouble
    }
    (perBank, perBank.kahanSum)

  /** Apply bail-in: haircut uninsured deposits on newly failed banks. Returns (updated banks, total bail-in loss).
    */
  def applyBailIn(banks: Vector[BankState]): (Vector[BankState], Double) =
    if !sfc.config.Config.BailInEnabled then (banks, 0.0)
    else
      var totalBailIn = 0.0
      val updated = banks.map { b =>
        if b.failed && b.deposits > PLN.Zero then
          val guaranteed = b.deposits.min(PLN(sfc.config.Config.BfgDepositGuarantee))
          val uninsured = b.deposits - guaranteed
          val haircut = uninsured * sfc.config.Config.BailInDepositHaircut
          totalBailIn += haircut.toDouble
          b.copy(deposits = b.deposits - haircut)
        else b
      }
      (updated, totalBailIn)

  /** BFG resolution: transfer deposits, bonds, performing loans, consumer loans to healthiest bank. */
  def resolveFailures(banks: Vector[BankState]): (Vector[BankState], BankId) =
    val newlyFailed = banks.filter(b => b.failed && b.deposits > PLN.Zero)
    if newlyFailed.isEmpty then (banks, BankId.NoBank)
    else
      val absorberId = healthiestBankId(banks)
      val resolved = banks.map { b =>
        if b.id == absorberId then
          val toAbsorb = newlyFailed.filter(_.id != absorberId)
          val addedDeposits = toAbsorb.kahanSumBy(_.deposits.toDouble)
          val addedLoans = toAbsorb.kahanSumBy(f => (f.loans - f.nplAmount).toDouble)
          val addedBonds = toAbsorb.kahanSumBy(_.govBondHoldings.toDouble)
          val addedCorpBonds = toAbsorb.kahanSumBy(_.corpBondHoldings.toDouble)
          val addedConsumerLoans = toAbsorb.kahanSumBy(_.consumerLoans.toDouble)
          val addedInterbank = toAbsorb.kahanSumBy(_.interbankNet.toDouble)
          b.copy(
            deposits = b.deposits + PLN(addedDeposits),
            loans = b.loans + PLN(Math.max(0, addedLoans)),
            govBondHoldings = b.govBondHoldings + PLN(addedBonds),
            corpBondHoldings = b.corpBondHoldings + PLN(addedCorpBonds),
            consumerLoans = b.consumerLoans + PLN(addedConsumerLoans),
            interbankNet = b.interbankNet + PLN(addedInterbank),
            failed = false,
          )
        else if b.failed && b.deposits > PLN.Zero then
          b.copy(
            deposits = PLN.Zero,
            loans = PLN.Zero,
            govBondHoldings = PLN.Zero,
            nplAmount = PLN.Zero,
            interbankNet = PLN.Zero,
            reservesAtNbp = PLN.Zero,
            corpBondHoldings = PLN.Zero,
            consumerLoans = PLN.Zero,
            consumerNpl = PLN.Zero,
          )
        else b
      }
      (resolved, absorberId)

  /** Find the healthiest (highest CAR) surviving bank. */
  def healthiestBankId(banks: Vector[BankState]): BankId =
    val alive = banks.filterNot(_.failed)
    if alive.isEmpty then banks.maxBy(_.capital.toDouble).id
    else alive.maxBy(_.car).id

  /** Reassign a firm/household from a failed bank to the healthiest surviving bank. */
  def reassignBankId(currentBankId: BankId, banks: Vector[BankState]): BankId =
    if currentBankId.toInt < banks.length && !banks(currentBankId.toInt).failed then currentBankId
    else healthiestBankId(banks)

  /** Allocate new bond issuance to banks proportional to their deposits. */
  def allocateBonds(banks: Vector[BankState], deficit: Double): Vector[BankState] =
    if deficit == 0.0 then return banks
    val aliveBanks = banks.filterNot(_.failed)
    val nAlive = aliveBanks.length
    if nAlive == 0 then return banks
    val totalDep = aliveBanks.kahanSumBy(_.deposits.toDouble)
    val lastAliveId = aliveBanks.last.id
    var allocated = 0.0
    banks.map { b =>
      if b.failed then b
      else if b.id == lastAliveId then b.copy(govBondHoldings = b.govBondHoldings + PLN(deficit - allocated))
      else
        val share = if totalDep > 0 then b.deposits.toDouble / totalDep else 1.0 / nAlive
        val amount = deficit * share
        allocated += amount
        b.copy(govBondHoldings = b.govBondHoldings + PLN(amount))
    }

  /** Compute reserve interest for a single bank (monthly). */
  def reserveInterest(bank: BankState, refRate: Double): Double =
    if bank.failed || bank.reservesAtNbp <= PLN.Zero then 0.0
    else (bank.reservesAtNbp * refRate * sfc.config.Config.NbpReserveRateMult / 12.0).toDouble

  /** Compute reserve interest for all banks. Returns per-bank vector + total. */
  def computeReserveInterest(banks: Vector[BankState], refRate: Double): (Vector[Double], Double) =
    val perBank = banks.map(b => reserveInterest(b, refRate))
    (perBank, perBank.kahanSum)

  /** Compute standing facility flows for all banks (monthly). */
  def computeStandingFacilities(banks: Vector[BankState], refRate: Double): (Vector[Double], Double) =
    if !sfc.config.Config.NbpStandingFacilities then (banks.map(_ => 0.0), 0.0)
    else
      val depositRate = Math.max(0.0, refRate - sfc.config.Config.NbpDepositFacilitySpread)
      val lombardRate = refRate + sfc.config.Config.NbpLombardSpread
      val perBank = banks.map { b =>
        if b.failed then 0.0
        else if b.reservesAtNbp > PLN.Zero then (b.reservesAtNbp * depositRate / 12.0).toDouble
        else if b.interbankNet < PLN.Zero then
          (b.interbankNet.toDouble.abs * lombardRate / 12.0) * -1.0 // cost (negative income)
        else 0.0
      }
      (perBank, perBank.kahanSum)

  /** Compute interbank interest flows for all banks (monthly). Net zero in aggregate.
    */
  def interbankInterestFlows(banks: Vector[BankState], rate: Double): (Vector[Double], Double) =
    val perBank = banks.map { b =>
      if b.failed then 0.0
      else (b.interbankNet * rate / 12.0).toDouble
    }
    (perBank, perBank.kahanSum)

  /** Allocate QE purchases from banks proportional to their bond holdings. */
  def allocateQePurchases(banks: Vector[BankState], qeTotal: Double): Vector[BankState] =
    if qeTotal <= 0 then banks
    else
      val eligible = banks.filter(b => !b.failed && b.govBondHoldings > PLN.Zero)
      val totalBonds = eligible.kahanSumBy(_.govBondHoldings.toDouble)
      if totalBonds <= 0 then banks
      else
        val lastEligibleId = eligible.last.id
        var allocated = 0.0
        banks.map { b =>
          if b.failed || b.govBondHoldings <= PLN.Zero then b
          else if b.id == lastEligibleId then
            val residual = qeTotal - allocated
            val sold = b.govBondHoldings.min(PLN(residual))
            b.copy(govBondHoldings = b.govBondHoldings - sold)
          else
            val share = b.govBondHoldings.toDouble / totalBonds
            val sold = b.govBondHoldings.min(PLN(qeTotal * share))
            allocated += sold.toDouble
            b.copy(govBondHoldings = b.govBondHoldings - sold)
        }
