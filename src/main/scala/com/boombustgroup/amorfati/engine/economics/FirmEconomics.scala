package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.{OperationalSignals, World}
import com.boombustgroup.amorfati.engine.ledger.{CorporateBondOwnership, LedgerFinancialState}
import com.boombustgroup.amorfati.engine.markets.{CalvoPricing, CorporateBondMarket, IntermediateMarket, LaborMarket}
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

/** Firm sector economics — production, I-O intermediate market, CAPEX
  * decisions, financing splits (equity/bonds/bank loans), labor matching, NPL
  * detection.
  *
  * `runStep` is the single runtime entry point; flow emission reads the
  * returned `StepOutput` directly.
  */
object FirmEconomics:

  // ---- Calibration constants ----
  private val BondRevertThreshold = 0.001 // minimum revert ratio to trigger bond-to-loan reversion

  // ---- Accumulated flows (monoid on PLN) ----

  /** Accumulated monetary flows from firm processing — one per firm, reduced
    * via `+`.
    */
  private case class FirmFlows(
      tax: PLN,             // CIT paid (after informal evasion)
      capex: PLN,           // technology upgrade CAPEX (AI or hybrid)
      techImp: PLN,         // import content of CAPEX (forex demand)
      newLoans: PLN,        // new bank loans net of equity/bond splits
      equityIssuance: PLN,  // GPW equity raised this month
      grossInvestment: PLN, // physical capital investment
      bondIssuance: PLN,    // corporate bond issuance (pre-absorption)
      profitShifting: PLN,  // FDI profit shifting outflow
      fdiRepatriation: PLN, // FDI dividend repatriation outflow
      inventoryChange: PLN, // net inventory change (+ accumulation, - drawdown)
      citEvasion: PLN,      // CIT evaded via informal economy
      energyCost: PLN,      // total energy + ETS cost
      greenInvestment: PLN, // green capital investment
      principalRepaid: PLN, // firm loan principal repaid
  ):
    def +(o: FirmFlows): FirmFlows = FirmFlows(
      tax + o.tax,
      capex + o.capex,
      techImp + o.techImp,
      newLoans + o.newLoans,
      equityIssuance + o.equityIssuance,
      grossInvestment + o.grossInvestment,
      bondIssuance + o.bondIssuance,
      profitShifting + o.profitShifting,
      fdiRepatriation + o.fdiRepatriation,
      inventoryChange + o.inventoryChange,
      citEvasion + o.citEvasion,
      energyCost + o.energyCost,
      greenInvestment + o.greenInvestment,
      principalRepaid + o.principalRepaid,
    )

  private object FirmFlows:
    val zero: FirmFlows = FirmFlows(
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
    )

  // ---- Internal phase result types ----

  /** Per-bank lending rates and credit approval, shared across phases. */
  private case class LendingConditions(
      firmWorld: World,                       // world with current-month wages for firm decision-making
      executionMonth: ExecutionMonth,         // realized month currently being executed
      operationalSignals: OperationalSignals, // same-month demand / labor surface for incumbent firms
      rates: Vector[Rate],                    // per-bank lending rates
      bankCanLend: (Int, PLN) => Boolean,     // credit approval: (bankId, amount) => approved?
      nBanks: Int,                            // number of banks in multi-bank system
  )

  /** Per-firm processing outcome — immutable, one per firm. */
  private case class FirmOutcome(
      firm: Firm.State,           // updated firm state after decision + financing
      financialStocks: Firm.FinancialStocks,
      flows: FirmFlows,           // monetary flows from this firm
      realizedPostTaxProfit: PLN, // realized monthly profit after tax, floored at zero
      bankId: BankId,             // relationship bank (for per-bank aggregation)
      finalLoan: PLN,             // bank loan after equity/bond splits
      bondAmt: PLN,               // corporate bond issuance (pre-absorption)
      principalRepaid: PLN,       // scheduled loan principal repaid this month
  )

  /** Result of per-firm processing (phase 2). */
  private case class FirmProcessingResult(
      outcomes: Vector[FirmOutcome],    // per-firm immutable outcomes
      flows: FirmFlows,                 // aggregate flows (monoid sum)
      firmBondAmounts: Map[FirmId, PLN], // per-firm bond issuance for reversion
  )

  /** Result of bond absorption (phase 3). */
  private case class BondAbsorptionResult(
      firms: Vector[Firm.State],                  // firms after bond reversion (unsold → bank loans)
      financialStocks: Vector[Firm.FinancialStocks],
      ledgerFinancialState: LedgerFinancialState, // ledger with issuer-side corporate bond balances after accepted issuance
      sumNewLoans: PLN,                           // total new bank loans incl. reversion
      corpBondAbsorption: Share,                  // Catalyst absorption ratio (0-1)
      actualBondIssuance: PLN,                    // bonds issued after absorption constraint
      bondReversionByFirm: Map[FirmId, PLN],      // unsold bonds reverted to relationship-bank loans
  )

  /** Financing split: how a firm's CAPEX loan is divided across three channels.
    */
  private case class FinancingSplit(
      bankLoan: PLN,    // remainder after equity and bond channels
      equity: PLN,      // GPW equity issuance (large firms only)
      bonds: PLN,       // Catalyst corporate bonds (medium+ firms only)
      firm: Firm.State, // firm with updated bank debt/equity; corporate bonds stay in LedgerFinancialState
      financialStocks: Firm.FinancialStocks,
  )

  /** Result of the intermediate market phase with ledger-facing firm balances
    * updated by the same cash deltas as firm states.
    */
  private case class IntermediateResult(
      firms: Vector[Firm.State],
      financialStocks: Vector[Firm.FinancialStocks],
      totalPaid: PLN,
  )

  /** Result of NPL and interest computation (phase 6). */
  private case class NplResult(
      nplNew: PLN,                      // new NPL volume from bankruptcies
      nplLoss: PLN,                     // NPL loss net of recovery
      totalBondDefault: PLN,            // bond default from bankrupt firms
      firmDeaths: Int,                  // count of newly bankrupt firms
      intIncome: PLN,                   // aggregate bank interest income
      perBankNplDebt: Vector[PLN],      // NPL debt by bank index
      perBankIntIncome: Vector[PLN],    // interest income by bank index
      perBankWorkers: Vector[Int],      // worker count by bank index
      defaultedBondFirmIds: Set[FirmId], // firms whose issuer liability defaulted this month
  )

  // ---- Public I/O types ----

  /** Full step output — all fields previously in FirmProcessingStep.Output. */
  case class StepOutput(
      ioFirms: Vector[Firm.State],               // firms after I-O intermediate market
      households: Vector[Household.State],       // households after labor matching + immigration
      sumTax: PLN,                               // aggregate CIT paid
      sumCapex: PLN,                             // aggregate technology CAPEX
      sumTechImp: PLN,                           // aggregate technology imports
      sumNewLoans: PLN,                          // aggregate new bank loans (incl. bond reversion)
      sumEquityIssuance: PLN,                    // aggregate GPW equity raised
      sumGrossInvestment: PLN,                   // aggregate physical capital investment
      sumBondIssuance: PLN,                      // aggregate bond issuance (pre-absorption)
      sumProfitShifting: PLN,                    // aggregate FDI profit shifting
      sumFdiRepatriation: PLN,                   // aggregate FDI repatriation
      sumInventoryChange: PLN,                   // aggregate net inventory change
      sumCitEvasion: PLN,                        // aggregate CIT evasion
      sumEnergyCost: PLN,                        // aggregate energy + ETS cost
      sumGreenInvestment: PLN,                   // aggregate green capital investment
      totalIoPaid: PLN,                          // total intermediate goods payments
      nplNew: PLN,                               // new non-performing loan volume
      nplLoss: PLN,                              // NPL loss net of recovery
      totalBondDefault: PLN,                     // bond default from bankrupt firms
      firmDeaths: Int,                           // number of firms that went bankrupt
      intIncome: PLN,                            // aggregate bank interest income
      corpBondAbsorption: Share,                 // Catalyst absorption ratio (0-1)
      actualBondIssuance: PLN,                   // bond issuance after absorption constraint
      netMigration: Int,                         // net immigration (inflow - outflow)
      perBankNewLoans: Vector[PLN],              // new loans by bank index
      sumFirmPrincipal: PLN,                     // aggregate firm loan principal repaid
      perBankFirmPrincipal: Vector[PLN],         // firm principal repaid by bank index
      perBankNplDebt: Vector[PLN],               // NPL debt by bank index
      perBankIntIncome: Vector[PLN],             // interest income by bank index
      perBankWorkers: Vector[Int],               // worker count by bank index
      lendingRates: Vector[Rate],                // per-bank lending rates
      postFirmCrossSectorHires: Int,             // cross-sector hires in labor matching
      markupInflation: Rate,                     // Calvo: annualized revenue-weighted avg markup change
      sumRealizedPostTaxProfit: PLN,             // aggregate realized post-tax profits from Firm.process
      sumStateOwnedPostTaxProfit: PLN,           // aggregate realized post-tax profits of SOEs
      ledgerFinancialState: LedgerFinancialState, // ledger with firm corporate-bond issuer balances after firm stage
  )

  /** Run the full firm processing pipeline from stage outputs. */
  def runStep(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      ledgerFinancialState: LedgerFinancialState,
      s1: FiscalConstraintEconomics.Output,
      s2: LaborEconomics.Output,
      s3: HouseholdIncomeEconomics.Output,
      s4: DemandEconomics.Output,
      rng: RandomStream,
  )(using p: SimParams): StepOutput =
    val stepIn = StepInput(w, firms, households, banks, ledgerFinancialState, s1, s2, s3, s4)
    runInternal(stepIn, rng)

  // ---- Core pipeline ----

  private def runInternal(stepIn: StepInput, rng: RandomStream)(using p: SimParams): StepOutput =
    val lending             = prepareLending(stepIn, rng)
    val fp                  = processFirms(stepIn.firms, stepIn.ledgerFinancialState, lending, rng)
    val bonded              = applyBondAbsorption(fp, stepIn.w, stepIn.banks, stepIn.ledgerFinancialState, lending.executionMonth)
    val intermediate        = applyIntermediateMarket(bonded.firms, bonded.financialStocks, stepIn)
    // Calvo staggered pricing: per-firm markup update
    val calvoFirms          = intermediate.firms.map: f =>
      val sectorMult         = stepIn.s4.sectorMults(f.sector.toInt)
      val passthrough        =
        if f.stateOwned then StateOwned.effectiveEnergyPassthrough(f.sector.toInt)
        else Share.One
      val energyCostPressure =
        CalvoPricing.energyCostPressure(
          stepIn.w.external.gvc.commodityPriceIndex,
          p.climate.energyCostShares(f.sector.toInt),
          passthrough,
        )
      val calvo              = CalvoPricing.updateFirmMarkup(f.markup, sectorMult, stepIn.s2.wageGrowth, energyCostPressure, rng)
      f.copy(markup = calvo.newMarkup)
    val laborMarket         = processLaborMarket(calvoFirms, stepIn, rng)
    val npl                 = computeNplAndInterest(
      stepIn.firms,
      calvoFirms,
      stepIn.ledgerFinancialState.firms.map(LedgerFinancialState.projectFirmFinancialStocks),
      intermediate.financialStocks,
      bonded.ledgerFinancialState,
      lending,
    )
    val issuerSettledLedger = bonded.ledgerFinancialState.copy(
      firms = CorporateBondOwnership.clearDefaultedIssuerDebt(bonded.ledgerFinancialState.firms, npl.defaultedBondFirmIds),
    )
    val markupInfl          = CalvoPricing.aggregateMarkupInflation(calvoFirms, intermediate.firms).annualize
    assembleOutput(
      fp,
      bonded,
      calvoFirms,
      intermediate.financialStocks,
      issuerSettledLedger,
      intermediate.totalPaid,
      laborMarket.households,
      laborMarket.crossSectorHires,
      laborMarket.newHouseholdFinancialStocksById,
      npl,
      stepIn,
      lending,
      markupInfl,
    )

  // ---- Internal step input ----

  private case class StepInput(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      ledgerFinancialState: LedgerFinancialState,
      s1: FiscalConstraintEconomics.Output,
      s2: LaborEconomics.Output,
      s3: HouseholdIncomeEconomics.Output,
      s4: DemandEconomics.Output,
  )

  private case class LaborMarketResult(
      households: Vector[Household.State],
      crossSectorHires: Int,
      newHouseholdFinancialStocksById: Map[HhId, Household.FinancialStocks],
  )

  // ---- Phase 1: Lending conditions ----

  /** Prepare per-bank rates, lending functions, and world snapshot with updated
    * wages for firm decision-making.
    */
  private def prepareLending(in: StepInput, rng: RandomStream)(using p: SimParams): LendingConditions =
    val bsec               = in.w.bankingSector
    val nBanks             = in.banks.length
    val ccyb               = in.w.mechanisms.macropru.ccyb
    val bankStocks         = in.ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks)
    val bankCorpBonds      = (bankId: BankId) => CorporateBondOwnership.bankHolderFor(in.ledgerFinancialState, bankId)
    val rates              = in.banks.zip(bankStocks).zip(bsec.configs).map { case ((b, stocks), cfg) =>
      Banking.lendingRate(b, stocks, cfg, in.s1.lendingBaseRate, in.w.gov.bondYield, bankCorpBonds(b.id))
    }
    val canLend            = (bankId: Int, amt: PLN) => Banking.canLend(in.banks(bankId), bankStocks(bankId), amt, rng, ccyb, bankCorpBonds(BankId(bankId)))
    val operationalSignals = OperationalSignals(
      sectorDemandMult = in.s4.sectorMults,
      sectorDemandPressure = in.s4.sectorDemandPressure,
      sectorHiringSignal = in.s4.sectorHiringSignal,
      operationalHiringSlack = in.s2.operationalHiringSlack,
    )
    val world              = in.w.copy(
      householdMarket = in.w.householdMarket.copy(
        marketWage = in.s2.newWage,
        reservationWage = in.s1.resWage,
      ),
    )
    LendingConditions(world, in.s1.m, operationalSignals, rates, canLend, nBanks)

  // ---- Phase 2: Per-firm processing ----

  /** Process each firm: technology decisions, financing splits (equity → bonds
    * → bank loans). Returns immutable per-firm outcomes and aggregate flows.
    */
  private def processFirms(
      firms: Vector[Firm.State],
      ledgerFinancialState: LedgerFinancialState,
      lending: LendingConditions,
      rng: RandomStream,
  )(using p: SimParams): FirmProcessingResult =
    require(
      firms.length == ledgerFinancialState.firms.length,
      s"FirmEconomics.processFirms requires aligned firms and ledger firm balances, got ${firms.length} firms and ${ledgerFinancialState.firms.length} balance rows",
    )
    val openingStocks = ledgerFinancialState.firms.map(LedgerFinancialState.projectFirmFinancialStocks)
    val outcomes      = firms
      .zip(openingStocks)
      .map: (f, stocks) =>
        val rate    = lending.rates(f.bankId.toInt)
        val canLend = (amt: PLN) => lending.bankCanLend(f.bankId.toInt, amt)
        val r       = Firm.process(
          f,
          stocks,
          lending.firmWorld,
          lending.executionMonth,
          lending.operationalSignals,
          rate,
          canLend,
          firms,
          rng,
          CorporateBondOwnership.issuerBalanceFor(ledgerFinancialState, f.id),
        )
        val fin     = splitFinancing(r)

        FirmOutcome(
          firm = fin.firm,
          financialStocks = fin.financialStocks,
          flows = FirmFlows(
            tax = r.taxPaid,
            capex = r.capexSpent,
            techImp = r.techImports,
            newLoans = fin.bankLoan,
            equityIssuance = fin.equity,
            grossInvestment = r.grossInvestment,
            bondIssuance = fin.bonds,
            profitShifting = r.profitShiftCost,
            fdiRepatriation = r.fdiRepatriation,
            inventoryChange = r.inventoryChange,
            citEvasion = r.citEvasion,
            energyCost = r.energyCost,
            greenInvestment = r.greenInvestment,
            principalRepaid = r.principalRepaid,
          ),
          realizedPostTaxProfit = r.realizedPostTaxProfit,
          bankId = f.bankId,
          finalLoan = fin.bankLoan,
          bondAmt = fin.bonds,
          principalRepaid = r.principalRepaid,
        )

    val aggFlows = outcomes.foldLeft(FirmFlows.zero)((acc, o) => acc + o.flows)
    val bondMap  = outcomes.collect { case o if o.bondAmt > PLN.Zero => o.firm.id -> o.bondAmt }.toMap

    FirmProcessingResult(outcomes, aggFlows, bondMap)

  /** Split a firm's new loan into three channels: GPW equity → Catalyst bonds →
    * bank loan remainder.
    */
  private def splitFinancing(r: Firm.Result)(using p: SimParams): FinancingSplit =
    // Channel 1: GPW equity issuance (large firms only)
    val (afterEquityLoan, equityAmt, afterEquityFirm, afterEquityStocks) =
      if r.newLoan > PLN.Zero &&
        Firm.workerCount(r.firm) >= p.equity.issuanceMinSize
      then
        val eq     = r.newLoan * p.equity.issuanceFrac
        val adj    = r.newLoan - eq
        val stocks = r.financialStocks.copy(
          firmLoan = r.financialStocks.firmLoan - eq,
          equity = r.financialStocks.equity + eq,
        )
        (adj, eq, r.firm, stocks)
      else (r.newLoan, PLN.Zero, r.firm, r.financialStocks)

    // Channel 2: Catalyst corporate bonds (medium+ firms only)
    val (finalLoan, bondAmt, finalFirm, finalStocks) =
      if afterEquityLoan > PLN.Zero && Firm.workerCount(afterEquityFirm) >= p.corpBond.minSize then
        val ba     = afterEquityLoan * p.corpBond.issuanceFrac
        val adj    = afterEquityLoan - ba
        val stocks = afterEquityStocks.copy(firmLoan = afterEquityStocks.firmLoan - ba)
        (adj, ba, afterEquityFirm, stocks)
      else (afterEquityLoan, PLN.Zero, afterEquityFirm, afterEquityStocks)

    FinancingSplit(finalLoan, equityAmt, bondAmt, finalFirm, finalStocks)

  // ---- Phase 3: Bond absorption ----

  /** Apply Catalyst demand-side absorption constraint. Unsold bonds revert to
    * bank loans on the issuing firm's relationship bank.
    */
  private def applyBondAbsorption(
      result: FirmProcessingResult,
      w: World,
      banks: Vector[Banking.BankState],
      ledgerFinancialState: LedgerFinancialState,
      executionMonth: ExecutionMonth,
  )(using p: SimParams): BondAbsorptionResult =
    val bankAgg              = Banking.aggregateFromBankStocks(
      banks,
      ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks),
      bankId => CorporateBondOwnership.bankHolderFor(ledgerFinancialState, bankId),
    )
    val absorption           = CorporateBondMarket
      .computeAbsorption(w.financialMarkets.corporateBonds, result.flows.bondIssuance, bankAgg.car, p.banking.minCar)
    val actualBondIssuance   = result.flows.bondIssuance * absorption
    val revertShare          = Share.One - absorption
    val requestedByFirm      = result.outcomes
      .map(o => o.firm.id -> result.firmBondAmounts.getOrElse(o.firm.id, PLN.Zero))
      .filter((_, amount) => amount > PLN.Zero)
    val actualIssuanceByFirm =
      allocateAbsorbedBondIssuance(requestedByFirm, actualBondIssuance, executionMonth)
    val shouldRevert         = revertShare > Share(BondRevertThreshold)
    val bondReversionByFirm  =
      if shouldRevert then
        requestedByFirm
          .map((firmId, requested) => firmId -> (requested - actualIssuanceByFirm.getOrElse(firmId, PLN.Zero)).max(PLN.Zero))
          .filter((_, amount) => amount > PLN.Zero)
          .toMap
      else Map.empty[FirmId, PLN]

    val adjusted =
      result.outcomes.map: o =>
        val revert = bondReversionByFirm.getOrElse(o.firm.id, PLN.Zero)
        if revert > PLN.Zero then
          (
            o.firm,
            o.financialStocks.copy(firmLoan = o.financialStocks.firmLoan + revert),
          )
        else (o.firm, o.financialStocks)

    val issuerLedger = ledgerFinancialState.copy(
      firms = CorporateBondOwnership.applyIssuance(ledgerFinancialState.firms, actualIssuanceByFirm),
    )

    val bondRevertLoans = bondReversionByFirm.valuesIterator.sum

    BondAbsorptionResult(
      adjusted.map(_._1),
      adjusted.map(_._2),
      issuerLedger,
      result.flows.newLoans + bondRevertLoans,
      absorption,
      actualBondIssuance,
      bondReversionByFirm,
    )

  private[amorfati] def allocateAbsorbedBondIssuance(
      requestedByFirm: Vector[(FirmId, PLN)],
      actualBondIssuance: PLN,
      executionMonth: ExecutionMonth = ExecutionMonth.First,
  ): Map[FirmId, PLN] =
    val positiveRequests = requestedByFirm.filter((_, amount) => amount > PLN.Zero)
    val target           = actualBondIssuance.distributeRaw
    val totalRequested   = positiveRequests.iterator.map((_, amount) => amount.distributeRaw).sum
    if positiveRequests.isEmpty || target <= 0L || totalRequested <= 0L then Map.empty
    else if target >= totalRequested then positiveRequests.toMap
    else
      case class AllocationRow(index: Int, firmId: FirmId, requested: Long, base: Long, remainder: BigInt, tieBreak: Long)

      val rows = positiveRequests.zipWithIndex.map { case ((firmId, requestedAmount), index) =>
        val requested = requestedAmount.distributeRaw
        val product   = BigInt(target) * BigInt(requested)
        AllocationRow(
          index = index,
          firmId = firmId,
          requested = requested,
          base = (product / BigInt(totalRequested)).toLong,
          remainder = product % BigInt(totalRequested),
          tieBreak = allocationTieBreak(firmId, executionMonth),
        )
      }

      val remaining         = target - rows.iterator.map(_.base).sum
      val (_, bonusByIndex) = rows
        .sortWith: (left, right) =>
          if left.remainder == right.remainder then
            if left.tieBreak == right.tieBreak then left.index < right.index
            else left.tieBreak < right.tieBreak
          else left.remainder > right.remainder
        .foldLeft((remaining, Map.empty[Int, Long])) { case ((left, bonuses), row) =>
          if left <= 0L || row.base >= row.requested then (left, bonuses)
          else (left - 1L, bonuses.updated(row.index, 1L))
        }

      val allocations  = rows.map: row =>
        row.firmId -> PLN.fromRaw(row.base + bonusByIndex.getOrElse(row.index, 0L))
      val allocatedRaw = allocations.iterator.map((_, amount) => amount.distributeRaw).sum
      require(
        allocatedRaw == target,
        s"Corporate bond absorption allocation must sum to target=$target, got $allocatedRaw",
      )
      allocations.filter((_, amount) => amount > PLN.Zero).toMap

  private def allocationTieBreak(firmId: FirmId, executionMonth: ExecutionMonth): Long =
    val firm  = firmId.toInt.toLong
    val month = executionMonth.toLong
    val mixed = (firm * 0x9e3779b97f4a7c15L) ^ (month * 0xbf58476d1ce4e5b9L)
    val step1 = (mixed ^ (mixed >>> 30)) * 0xbf58476d1ce4e5b9L
    val step2 = (step1 ^ (step1 >>> 27)) * 0x94d049bb133111ebL
    (step2 ^ (step2 >>> 31)) & Long.MaxValue

  // ---- Phase 4: Intermediate market ----

  /** Run I-O intermediate goods market. Adjusts firm cash positions (zero-sum
    * transfers between sectors). Returns updated firms and total paid.
    */
  private def applyIntermediateMarket(
      firms: Vector[Firm.State],
      financialStocks: Vector[Firm.FinancialStocks],
      in: StepInput,
  )(using p: SimParams): IntermediateResult =

    val r              = IntermediateMarket.process(
      IntermediateMarket.Input(
        firms = firms,
        sectorMults = in.s4.sectorMults,
        price = in.w.priceLevel,
        ioMatrix = p.io.matrix,
        columnSums = p.io.columnSums,
        scale = p.io.scale,
      ),
    )
    val adjustedStocks = financialStocks
      .zip(r.cashAdjustments)
      .map: (stocks, cashAdjustment) =>
        stocks.copy(cash = stocks.cash + cashAdjustment)
    IntermediateResult(r.firms, adjustedStocks, r.totalPaid)

  // ---- Phase 5: Labor market + immigration ----

  /** Run labor market: separate displaced workers, match unemployed to
    * vacancies (skill-ranked), update wages, process immigration flows.
    */
  private def processLaborMarket(
      ioFirms: Vector[Firm.State],
      in: StepInput,
      rng: RandomStream,
  )(using p: SimParams): LaborMarketResult =
    val afterSep     = LaborMarket.separations(in.s3.updatedHouseholds, in.firms, ioFirms)
    val searchResult = LaborMarket.jobSearch(afterSep, ioFirms, in.s2.newWage, rng, in.s2.regionalWages)
    val postWages    = LaborMarket.updateWages(searchResult.households, ioFirms, in.s2.newWage)

    val afterRemoval    = Immigration.removeReturnMigrants(postWages, in.s2.newImmig.monthlyOutflow)
    val startId         = afterRemoval.map(_.id.toInt).maxOption.getOrElse(-1) + 1
    val newImmigrants   = Immigration.spawnImmigrantPopulation(in.s2.newImmig.monthlyInflow, startId, rng)
    val finalHouseholds = afterRemoval ++ newImmigrants.households
    val newStocksById   = newImmigrants.households.zip(newImmigrants.financialStocks).map((household, stocks) => household.id -> stocks).toMap

    LaborMarketResult(finalHouseholds, searchResult.crossSectorHires, newStocksById)

  // ---- Phase 6: NPL and interest income ----

  /** Detect newly bankrupt firms, compute per-bank NPL losses and interest
    * income on pre-step debt stock.
    */
  private def computeNplAndInterest(
      preFirms: Vector[Firm.State],
      postFirms: Vector[Firm.State],
      openingFinancialStocks: Vector[Firm.FinancialStocks],
      closingFinancialStocks: Vector[Firm.FinancialStocks],
      ledgerFinancialState: LedgerFinancialState,
      lending: LendingConditions,
  )(using p: SimParams): NplResult =
    val prevAlive            = preFirms.filter(Firm.isAlive).map(_.id).toSet
    val newlyDead            = postFirms.filter(f => !Firm.isAlive(f) && prevAlive.contains(f.id))
    val closingStocksById    = postFirms.zip(closingFinancialStocks).map((firm, stocks) => firm.id -> stocks).toMap
    val openingStocksById    = preFirms.zip(openingFinancialStocks).map((firm, stocks) => firm.id -> stocks).toMap
    val nplNew               = newlyDead.flatMap(f => closingStocksById.get(f.id).map(_.firmLoan)).sum
    val nplLoss              = nplNew * (Share.One - p.banking.loanRecovery)
    val defaultedBondFirmIds = newlyDead.map(_.id).toSet
    val totalBondDefault     = CorporateBondOwnership.defaultedIssuerDebt(ledgerFinancialState.firms, defaultedBondFirmIds)

    // Per-bank aggregation via groupMapReduce (pure, no mutable arrays)
    val emptyPln  = Vector.fill(lending.nBanks)(PLN.Zero)
    val emptyInts = Vector.fill(lending.nBanks)(0)

    val perBankNplDebt = newlyDead.foldLeft(emptyPln): (acc, f) =>
      acc.updated(f.bankId.toInt, acc(f.bankId.toInt) + closingStocksById.get(f.id).fold(PLN.Zero)(_.firmLoan))

    val perBankIntIncome = preFirms
      .filter(Firm.isAlive)
      .foldLeft(emptyPln): (acc, f) =>
        val interest = openingStocksById.get(f.id).fold(PLN.Zero)(_.firmLoan) * lending.rates(f.bankId.toInt).monthly
        acc.updated(f.bankId.toInt, acc(f.bankId.toInt) + interest)

    val perBankWorkers = postFirms
      .filter(Firm.isAlive)
      .foldLeft(emptyInts): (acc, f) =>
        acc.updated(f.bankId.toInt, acc(f.bankId.toInt) + Firm.workerCount(f))

    NplResult(
      nplNew,
      nplLoss,
      totalBondDefault,
      newlyDead.length,
      perBankIntIncome.foldLeft(PLN.Zero)(_ + _),
      perBankNplDebt,
      perBankIntIncome,
      perBankWorkers,
      defaultedBondFirmIds,
    )

  // ---- Output assembly ----

  /** Assemble final StepOutput from phase results. Pure mapping, no
    * computation.
    */
  private def assembleOutput(
      fp: FirmProcessingResult,
      bonded: BondAbsorptionResult,
      ioFirms: Vector[Firm.State],
      firmFinancialStocks: Vector[Firm.FinancialStocks],
      ledgerFinancialState: LedgerFinancialState,
      totalIoPaid: PLN,
      households: Vector[Household.State],
      crossSectorHires: Int,
      newHouseholdFinancialStocksById: Map[HhId, Household.FinancialStocks],
      npl: NplResult,
      in: StepInput,
      lending: LendingConditions,
      markupInflation: Rate,
  ): StepOutput =
    val flows                 = fp.flows
    val ledgerAfterFirmStocks = in.s3.ledgerFinancialState.copy(
      households = LedgerFinancialState.refreshHouseholdBalances(
        households,
        in.s3.updatedHouseholds,
        in.s3.ledgerFinancialState.households,
        newHouseholdFinancialStocksById,
      ),
      firms = LedgerFinancialState.refreshFirmFinancialBalances(firmFinancialStocks, ledgerFinancialState.firms),
    )

    // Derive per-bank vectors from immutable outcomes
    val emptyPln = Vector.fill(lending.nBanks)(PLN.Zero)

    val perBankNewLoans = fp.outcomes.foldLeft(emptyPln): (acc, o) =>
      acc.updated(o.bankId.toInt, acc(o.bankId.toInt) + o.finalLoan)

    // Add bond reversion amounts to per-bank loans
    val perBankNewLoansWithRevert =
      if bonded.corpBondAbsorption < Share.One then
        fp.outcomes.foldLeft(perBankNewLoans): (acc, o) =>
          val revert = bonded.bondReversionByFirm.getOrElse(o.firm.id, PLN.Zero)
          if revert > PLN.Zero then acc.updated(o.bankId.toInt, acc(o.bankId.toInt) + revert)
          else acc
      else perBankNewLoans

    val perBankFirmPrincipal = fp.outcomes.foldLeft(emptyPln): (acc, o) =>
      acc.updated(o.bankId.toInt, acc(o.bankId.toInt) + o.principalRepaid)

    StepOutput(
      ioFirms = ioFirms,
      households = households,
      sumTax = flows.tax,
      sumCapex = flows.capex,
      sumTechImp = flows.techImp,
      sumNewLoans = bonded.sumNewLoans,
      sumEquityIssuance = flows.equityIssuance,
      sumGrossInvestment = flows.grossInvestment,
      sumBondIssuance = flows.bondIssuance,
      sumProfitShifting = flows.profitShifting,
      sumFdiRepatriation = flows.fdiRepatriation,
      sumInventoryChange = flows.inventoryChange,
      sumCitEvasion = flows.citEvasion,
      sumEnergyCost = flows.energyCost,
      sumGreenInvestment = flows.greenInvestment,
      totalIoPaid = totalIoPaid,
      nplNew = npl.nplNew,
      nplLoss = npl.nplLoss,
      totalBondDefault = npl.totalBondDefault,
      firmDeaths = npl.firmDeaths,
      intIncome = npl.intIncome,
      corpBondAbsorption = bonded.corpBondAbsorption,
      actualBondIssuance = bonded.actualBondIssuance,
      netMigration = in.s2.newImmig.monthlyInflow - in.s2.newImmig.monthlyOutflow,
      perBankNewLoans = perBankNewLoansWithRevert,
      sumFirmPrincipal = flows.principalRepaid,
      perBankFirmPrincipal = perBankFirmPrincipal,
      perBankNplDebt = npl.perBankNplDebt,
      perBankIntIncome = npl.perBankIntIncome,
      perBankWorkers = npl.perBankWorkers,
      lendingRates = lending.rates,
      postFirmCrossSectorHires = crossSectorHires,
      markupInflation = markupInflation,
      sumRealizedPostTaxProfit = fp.outcomes.foldLeft(PLN.Zero)(_ + _.realizedPostTaxProfit),
      sumStateOwnedPostTaxProfit = fp.outcomes.filter(_.firm.stateOwned).foldLeft(PLN.Zero)((acc, o) => acc + o.realizedPostTaxProfit),
      ledgerFinancialState = ledgerAfterFirmStocks,
    )
