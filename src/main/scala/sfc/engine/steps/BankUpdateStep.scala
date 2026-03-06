package sfc.engine.steps

import sfc.accounting.{BankState, BopState, GovState, MonetaryAggregates}
import sfc.agents.*
import sfc.config.{Config, RunConfig, SECTORS}
import sfc.engine.*
import sfc.types.*
import sfc.util.KahanSum.*

object BankUpdateStep:

  case class Input(
    w: World,
    // Aggregate income/consumption
    consumption: Double,
    totalIncome: Double,
    domesticCons: Double,
    employed: Int,
    newWage: Double,
    resWage: Double,
    // Step 1 outputs
    bdpActive: Boolean,
    bdp: Double,
    lendingBaseRate: Double,
    // Step 2 outputs
    newZus: SocialSecurity.ZusState,
    newPpk: SocialSecurity.PpkState,
    rawPpkBondPurchase: Double,
    wageGrowth: Double,
    // Step 3 outputs
    pitRevenue: Double,
    importAdj: Double,
    aggUnempBenefit: Double,
    perBankHhFlowsOpt: Option[PerBankHhFlows],
    hhAgg: Option[Household.Aggregates],
    // Step 4 outputs
    govPurchases: Double,
    laggedInvestDemand: Double,
    // Step 5 outputs
    ioFirms: Array[Firm.State],
    rewiredFirms: Array[Firm.State],
    finalHouseholds: Option[Vector[Household.State]],
    sumTax: Double,
    sumNewLoans: Double,
    nplNew: Double,
    nplLoss: Double,
    intIncome: Double,
    sumGrossInvestment: Double,
    sumGreenInvestment: Double,
    perBankNewLoans: Array[Double],
    perBankNplDebt: Array[Double],
    perBankIntIncome: Array[Double],
    perBankWorkers: Array[Int],
    // Step 6 outputs
    hhDebtService: Double,
    depositInterestPaid: Double,
    consumerDebtService: Double,
    consumerOrigination: Double,
    consumerDefaultAmt: Double,
    consumerNplLoss: Double,
    consumerPrincipal: Double,
    remittanceOutflow: Double,
    diasporaInflow: Double,
    tourismExport: Double,
    tourismImport: Double,
    // Step 7 outputs
    euCofin: Double,
    euProjectCapital: Double,
    gdp: Double,
    newMacropru: Macroprudential.State,
    newInfl: Double,
    newPrice: Double,
    netDomesticDividends: Double,
    foreignDividendOutflow: Double,
    dividendTax: Double,
    // Step 8 outputs
    newBop: BopState,
    totalReserveInterest: Double,
    totalStandingFacilityIncome: Double,
    totalInterbankInterest: Double,
    newBondYield: Double,
    monthlyDebtService: Double,
    bankBondIncome: Double,
    nbpRemittance: Double,
    postFxNbp: Nbp.State,
    qePurchaseAmount: Double,
    newCorpBonds: CorporateBondMarket.State,
    corpBondBankCoupon: Double,
    corpBondBankDefaultLoss: Double,
    newInsurance: Insurance.State,
    insNetDepositChange: Double,
    newNbfi: Nbfi.State,
    nbfiDepositDrain: Double,
    m: Int,
    rc: RunConfig,
  )

  case class Output(
    resolvedBank: BankState,
    finalBankingSector: Option[Banking.State],
    reassignedFirms: Array[Firm.State],
    reassignedHouseholds: Option[Vector[Household.State]],
    finalPpk: SocialSecurity.PpkState,
    finalInsurance: Insurance.State,
    finalNbfi: Nbfi.State,
    newGovWithYield: GovState,
    newJst: Jst.State,
    housingAfterFlows: HousingMarket.State,
    bfgLevy: Double,
    bailInLoss: Double,
    multiCapDestruction: Double,
    monAgg: Option[MonetaryAggregates],
    finalHhAgg: Option[Household.Aggregates],
    // Tax intermediates (needed by SFC check)
    vat: Double,
    vatAfterEvasion: Double,
    pitAfterEvasion: Double,
    exciseRevenue: Double,
    exciseAfterEvasion: Double,
    customsDutyRevenue: Double,
    effectiveShadowShare: Double,
    // Housing flows (needed by SFC check)
    mortgageInterestIncome: Double,
    mortgagePrincipal: Double,
    mortgageDefaultLoss: Double,
    mortgageDefaultAmount: Double,
    // Other intermediates (needed by SFC/World assembly)
    jstDepositChange: Double,
    investNetDepositFlow: Double,
    actualBondChange: Double,
  )

  def run(in: Input): Output =
    val vat = in.consumption * Config.FofConsWeights.zip(Config.VatRates).map((w, r) => w * r).kahanSum
    val exciseRevenue = in.consumption * Config.FofConsWeights.zip(Config.ExciseRates).map((w, r) => w * r).kahanSum
    val customsDutyRevenue =
      if Config.OeEnabled then in.newBop.totalImports.toDouble * Config.CustomsNonEuShare * Config.CustomsDutyRate
      else 0.0
    val unempBenefitSpend = in.hhAgg.map(_.totalUnempBenefits.toDouble).getOrElse(in.aggUnempBenefit)
    val socialTransferSpend =
      if Config.Social800Enabled then
        in.hhAgg
          .map(_.totalSocialTransfers.toDouble)
          .getOrElse(
            Config.TotalPopulation.toDouble * Config.Social800ChildrenPerHh * Config.Social800Rate,
          )
      else 0.0

    // Informal economy: aggregate tax evasion (#45)
    val informalCyclicalAdj = in.w.informalCyclicalAdj
    val effectiveShadowShare =
      if Config.InformalEnabled then
        Config.FofConsWeights
          .zip(Config.InformalSectorShares)
          .map((cw, ss) => cw * Math.min(1.0, ss + informalCyclicalAdj))
          .kahanSum
      else 0.0
    val vatAfterEvasion =
      if Config.InformalEnabled then vat * (1.0 - effectiveShadowShare * Config.InformalVatEvasion) else vat
    val exciseAfterEvasion =
      if Config.InformalEnabled then exciseRevenue * (1.0 - effectiveShadowShare * Config.InformalExciseEvasion)
      else exciseRevenue
    val pitAfterEvasion =
      if Config.InformalEnabled then in.pitRevenue * (1.0 - effectiveShadowShare * Config.InformalPitEvasion)
      else in.pitRevenue

    val newGov = Sectors.updateGov(
      in.w.gov,
      in.sumTax + in.dividendTax + pitAfterEvasion,
      vatAfterEvasion,
      in.bdpActive,
      in.bdp,
      in.newPrice,
      unempBenefitSpend,
      in.monthlyDebtService,
      in.nbpRemittance,
      in.newZus.govSubvention.toDouble,
      socialTransferSpend,
      euCofinancing = in.euCofin,
      euProjectCapital = in.euProjectCapital,
      exciseRevenue = exciseAfterEvasion,
      customsDutyRevenue = customsDutyRevenue,
      govPurchasesActual = in.govPurchases,
    )
    val newGovWithYield = newGov.copy(bondYield = Rate(in.newBondYield))

    // JST (local government)
    val nLivingFirms = in.ioFirms.count(Firm.isAlive)
    val (newJst, jstDepositChange) =
      Jst.step(in.w.jst, newGovWithYield.taxRevenue.toDouble, in.totalIncome, in.gdp, nLivingFirms, pitAfterEvasion)

    // ---- Housing market step ----
    val unempRate = 1.0 - in.employed.toDouble / Config.TotalPopulation
    val prevMortgageRate = in.w.housing.avgMortgageRate
    val mortgageBaseRate: Double =
      if Config.InterbankTermStructure then
        in.w.bankingSector
          .map(bs => YieldCurve.compute(bs.interbankRate.toDouble).wibor3m.toDouble)
          .getOrElse(in.w.nbp.referenceRate.toDouble)
      else in.w.nbp.referenceRate.toDouble
    val mortgageRate = mortgageBaseRate + Config.ReMortgageSpread

    val housingAfterPrice =
      HousingMarket.step(in.w.housing, mortgageRate, in.newInfl, in.wageGrowth, in.employed, prevMortgageRate.toDouble)
    val housingAfterOrig = HousingMarket.processOrigination(housingAfterPrice, in.totalIncome, mortgageRate, true)
    val (mortgageInterestIncome, mortgagePrincipal, mortgageDefaultLoss) =
      HousingMarket.processMortgageFlows(housingAfterOrig, mortgageRate, unempRate)
    val mortgageDefaultAmount =
      if Config.ReMortgageRecovery < 1.0 then mortgageDefaultLoss / (1.0 - Config.ReMortgageRecovery)
      else 0.0
    val housingAfterFlows =
      HousingMarket.applyFlows(housingAfterOrig, mortgagePrincipal, mortgageDefaultAmount, mortgageInterestIncome)

    // BFG levy (#48)
    val bfgLevy =
      if Config.BankFailureEnabled then
        in.w.bankingSector match
          case Some(bs) =>
            val (_, total) = Banking.computeBfgLevy(bs.banks)
            total
          case None =>
            in.w.bank.deposits.toDouble * Config.BfgLevyRate / 12.0
      else 0.0

    // Investment net deposit flow
    val currentInvestDomestic = in.sumGrossInvestment * (1.0 - Config.PhysCapImportShare) +
      in.sumGreenInvestment * (1.0 - Config.GreenImportShare)
    val investNetDepositFlow = in.laggedInvestDemand - currentInvestDomestic

    val newBank = in.w.bank.copy(
      totalLoans = PLN(Math.max(0, in.w.bank.totalLoans.toDouble + in.sumNewLoans - in.nplNew * Config.LoanRecovery)),
      nplAmount = PLN(Math.max(0, in.w.bank.nplAmount.toDouble + in.nplNew - in.w.bank.nplAmount.toDouble * 0.05)),
      capital = PLN(
        in.w.bank.capital.toDouble - in.nplLoss - mortgageDefaultLoss - in.consumerNplLoss
          - in.corpBondBankDefaultLoss - bfgLevy
          + in.intIncome * 0.3 + in.hhDebtService * 0.3
          + in.bankBondIncome * 0.3 - in.depositInterestPaid * 0.3
          + in.totalReserveInterest * 0.3 + in.totalStandingFacilityIncome * 0.3
          + in.totalInterbankInterest * 0.3
          + mortgageInterestIncome * 0.3
          + in.consumerDebtService * 0.3
          + in.corpBondBankCoupon * 0.3,
      ),
      deposits = PLN(
        in.w.bank.deposits.toDouble + (in.totalIncome - in.consumption) + investNetDepositFlow
          + jstDepositChange
          + in.netDomesticDividends - in.foreignDividendOutflow - in.remittanceOutflow + in.diasporaInflow
          + in.tourismExport - in.tourismImport
          + in.consumerOrigination + in.insNetDepositChange + in.nbfiDepositDrain,
      ),
      consumerLoans = PLN(
        Math.max(
          0.0,
          in.w.bank.consumerLoans.toDouble + in.consumerOrigination - in.consumerPrincipal - in.consumerDefaultAmt,
        ),
      ),
      consumerNpl = PLN(
        Math.max(0.0, in.w.bank.consumerNpl.toDouble + in.consumerDefaultAmt - in.w.bank.consumerNpl.toDouble * 0.05),
      ),
      corpBondHoldings = in.newCorpBonds.bankHoldings,
    )

    // Recompute hhAgg from final households if in individual mode
    val monthlyRetAttempts = in.hhAgg.map(_.retrainingAttempts).getOrElse(0)
    val monthlyRetSuccesses = in.hhAgg.map(_.retrainingSuccesses).getOrElse(0)
    val finalHhAgg = in.finalHouseholds.map { hhs =>
      Household.computeAggregates(
        hhs,
        in.newWage,
        in.resWage,
        in.importAdj,
        monthlyRetAttempts,
        monthlyRetSuccesses,
        in.bdp,
      )
    }

    val actualBondChange = (newGovWithYield.bondsOutstanding - in.w.gov.bondsOutstanding).toDouble

    // PPK bond purchases (capped at available bonds after QE)
    val availableBondsForPpk = newBank.govBondHoldings.toDouble +
      (if Config.GovBondMarket then actualBondChange else 0.0) - in.qePurchaseAmount
    val ppkBondPurchase = Math.min(in.rawPpkBondPurchase, Math.max(0.0, availableBondsForPpk))
    val finalPpk = in.newPpk.copy(bondHoldings = PLN(in.w.ppk.bondHoldings.toDouble + ppkBondPurchase))

    // Insurance gov bond purchases (capped at available bonds after QE + PPK)
    val insGovBondDelta = (in.newInsurance.govBondHoldings - in.w.insurance.govBondHoldings).toDouble
    val availableBondsForIns = newBank.govBondHoldings.toDouble +
      (if Config.GovBondMarket then actualBondChange else 0.0) - in.qePurchaseAmount - ppkBondPurchase
    val insBondPurchase = Math.max(0.0, Math.min(Math.max(0.0, insGovBondDelta), Math.max(0.0, availableBondsForIns)))
    val finalInsurance =
      in.newInsurance.copy(govBondHoldings = PLN(in.w.insurance.govBondHoldings.toDouble + insBondPurchase))

    // TFI gov bond purchases (#42)
    val tfiGovBondDelta = (in.newNbfi.tfiGovBondHoldings - in.w.nbfi.tfiGovBondHoldings).toDouble
    val availableBondsForTfi = newBank.govBondHoldings.toDouble +
      (if Config.GovBondMarket then actualBondChange else 0.0) -
      in.qePurchaseAmount - ppkBondPurchase - insBondPurchase
    val tfiBondPurchase = Math.max(0.0, Math.min(Math.max(0.0, tfiGovBondDelta), Math.max(0.0, availableBondsForTfi)))
    val finalNbfi = in.newNbfi.copy(tfiGovBondHoldings = PLN(in.w.nbfi.tfiGovBondHoldings.toDouble + tfiBondPurchase))

    // Bond allocation: new issuance goes to bank; QE, PPK, insurance, TFI transfer from bank
    val finalBank = if Config.GovBondMarket then
      newBank.copy(govBondHoldings =
        PLN(
          newBank.govBondHoldings.toDouble + actualBondChange - in.qePurchaseAmount - ppkBondPurchase - insBondPurchase - tfiBondPurchase,
        ),
      )
    else
      newBank.copy(govBondHoldings =
        PLN(
          newBank.govBondHoldings.toDouble - in.qePurchaseAmount - ppkBondPurchase - insBondPurchase - tfiBondPurchase,
        ),
      )

    // ---- Multi-bank update path ----
    val (perBankReserveInt, perBankStandingFac, perBankInterbankInt) =
      in.w.bankingSector match
        case Some(bs) =>
          val (ri, _) = Banking.computeReserveInterest(bs.banks, in.w.nbp.referenceRate.toDouble)
          val (sf, _) = Banking.computeStandingFacilities(bs.banks, in.w.nbp.referenceRate.toDouble)
          val (ib, _) = Banking.interbankInterestFlows(bs.banks, bs.interbankRate.toDouble)
          (ri, sf, ib)
        case None =>
          (Vector.empty[Double], Vector.empty[Double], Vector.empty[Double])

    val (finalBankingSector, reassignedFirms, reassignedHouseholds, bailInLoss, multiCapDestruction) =
      in.w.bankingSector match
        case Some(bs) =>
          val totalWorkers = in.perBankWorkers.kahanSumBy(_.toDouble)
          val updatedBanks = bs.banks.map { b =>
            val bId = b.id.toInt
            val bankNplNew = in.perBankNplDebt(bId)
            val bankNplLoss = bankNplNew * (1.0 - Config.LoanRecovery)
            val bankIntIncome = in.perBankIntIncome(bId)
            val (
              bankIncomeShare,
              bankConsShare,
              bankHhDebtService,
              bankDepInterest,
              bankCcDSvc,
              bankCcOrig,
              bankCcDef,
            ) =
              in.perBankHhFlowsOpt match
                case Some(pbf) =>
                  (
                    pbf.income(bId),
                    pbf.consumption(bId),
                    pbf.debtService(bId),
                    pbf.depositInterest(bId),
                    if pbf.consumerDebtService.nonEmpty then pbf.consumerDebtService(bId) else 0.0,
                    if pbf.consumerOrigination.nonEmpty then pbf.consumerOrigination(bId) else 0.0,
                    if pbf.consumerDefault.nonEmpty then pbf.consumerDefault(bId) else 0.0,
                  )
                case None =>
                  val ws = if totalWorkers > 0 then in.perBankWorkers(bId) / totalWorkers else 0.0
                  (
                    in.totalIncome * ws,
                    in.consumption * ws,
                    in.hhDebtService * ws,
                    0.0,
                    in.consumerDebtService * ws,
                    in.consumerOrigination * ws,
                    in.consumerDefaultAmt * ws,
                  )
            val bankBondInc = b.govBondHoldings.toDouble * in.newBondYield / 12.0
            val bankResInt = if perBankReserveInt.nonEmpty then perBankReserveInt(bId) else 0.0
            val bankSfInc = if perBankStandingFac.nonEmpty then perBankStandingFac(bId) else 0.0
            val bankIbInt = if perBankInterbankInt.nonEmpty then perBankInterbankInt(bId) else 0.0
            val newLoansTotal =
              Math.max(0.0, b.loans.toDouble + in.perBankNewLoans(bId) - bankNplNew * Config.LoanRecovery)
            val ws = if totalWorkers > 0 then in.perBankWorkers(bId) / totalWorkers else 0.0
            val bankDivInflow = in.netDomesticDividends * ws
            val bankDivOutflow = in.foreignDividendOutflow * ws
            val bankRemittance = in.remittanceOutflow * ws
            val bankDiasporaInflow = in.diasporaInflow * ws
            val bankTourismExport = in.tourismExport * ws
            val bankTourismImport = in.tourismImport * ws
            val bankInsDepChange = in.insNetDepositChange * ws
            val bankNbfiDepDrain = in.nbfiDepositDrain * ws
            val bankJstDepChange = jstDepositChange * ws
            val bankInvestNetFlow = investNetDepositFlow * ws
            val newDep =
              b.deposits.toDouble + (bankIncomeShare - bankConsShare) + bankInvestNetFlow + bankJstDepChange + bankDivInflow - bankDivOutflow - bankRemittance + bankDiasporaInflow + bankTourismExport - bankTourismImport + bankCcOrig + bankInsDepChange + bankNbfiDepDrain
            val bankDepShare = if totalWorkers > 0 then in.perBankWorkers(bId) / totalWorkers else 0.0
            val bankMortgageIntIncome = mortgageInterestIncome * bankDepShare
            val bankMortgageNplLoss = mortgageDefaultLoss * bankDepShare
            val bankCcNplLoss = bankCcDef * (1.0 - Config.CcNplRecovery)
            val bankCcPrincipal = in.perBankHhFlowsOpt match
              case Some(pbf) if pbf.consumerPrincipal.nonEmpty => pbf.consumerPrincipal(bId)
              case _                                           =>
                if Config.CcAmortRate + (in.lendingBaseRate + Config.CcSpread) / 12.0 > 0 then
                  bankCcDSvc * (Config.CcAmortRate / (Config.CcAmortRate + (in.lendingBaseRate + Config.CcSpread) / 12.0))
                else 0.0
            val bankCorpBondCoupon = in.corpBondBankCoupon * bankDepShare
            val bankCorpBondDefaultLoss = in.corpBondBankDefaultLoss * bankDepShare
            val bankBfgLevy =
              if Config.BankFailureEnabled && !b.failed then b.deposits.toDouble * Config.BfgLevyRate / 12.0
              else 0.0
            b.copy(
              loans = PLN(newLoansTotal),
              nplAmount = PLN(Math.max(0.0, b.nplAmount.toDouble + bankNplNew - b.nplAmount.toDouble * 0.05)),
              capital = PLN(
                b.capital.toDouble - bankNplLoss - bankMortgageNplLoss - bankCcNplLoss
                  - bankCorpBondDefaultLoss - bankBfgLevy + bankIntIncome * 0.3 +
                  bankHhDebtService * 0.3 + bankBondInc * 0.3 - bankDepInterest * 0.3
                  + bankResInt * 0.3 + bankSfInc * 0.3 + bankIbInt * 0.3
                  + bankMortgageIntIncome * 0.3
                  + bankCcDSvc * 0.3
                  + bankCorpBondCoupon * 0.3,
              ),
              deposits = PLN(newDep),
              demandDeposits = PLN(newDep * (1.0 - Config.BankTermDepositFrac)),
              termDeposits = PLN(newDep * Config.BankTermDepositFrac),
              loansShort = PLN(newLoansTotal * 0.20),
              loansMedium = PLN(newLoansTotal * 0.30),
              loansLong = PLN(newLoansTotal * 0.50),
              consumerLoans = PLN(Math.max(0.0, b.consumerLoans.toDouble + bankCcOrig - bankCcPrincipal - bankCcDef)),
              consumerNpl = PLN(Math.max(0.0, b.consumerNpl.toDouble + bankCcDef - b.consumerNpl.toDouble * 0.05)),
              corpBondHoldings = in.newCorpBonds.bankHoldings * bankDepShare,
            )
          }
          val ibRate = Banking.interbankRate(updatedBanks, in.w.nbp.referenceRate.toDouble)
          val afterInterbank = Banking.clearInterbank(updatedBanks, bs.configs, ibRate)
          val afterBonds =
            if Config.GovBondMarket then Banking.allocateBonds(afterInterbank, actualBondChange)
            else afterInterbank
          val afterQe = Banking.allocateQePurchases(afterBonds, in.qePurchaseAmount)
          val afterPpk = Banking.allocateQePurchases(afterQe, ppkBondPurchase)
          val afterIns = Banking.allocateQePurchases(afterPpk, insBondPurchase)
          val afterTfi = Banking.allocateQePurchases(afterIns, tfiBondPurchase)
          val (afterFailCheck, anyFailed) = Banking.checkFailures(afterTfi, in.m, ccyb = in.newMacropru.ccyb.toDouble)
          val (afterBailIn, multiBailInLoss) =
            if anyFailed then Banking.applyBailIn(afterFailCheck) else (afterFailCheck, 0.0)
          val (afterResolve, rawAbsorberId) =
            if anyFailed then Banking.resolveFailures(afterBailIn)
            else (afterBailIn, BankId.NoBank)
          val absorberId =
            if rawAbsorberId.toInt >= 0 then rawAbsorberId
            else Banking.healthiestBankId(afterResolve)
          val multiCapDest =
            if anyFailed then
              afterTfi
                .zip(afterFailCheck)
                .map { (pre, post) =>
                  if !pre.failed && post.failed then pre.capital.toDouble else 0.0
                }
                .kahanSum
            else 0.0
          val curve = if Config.InterbankTermStructure then Some(YieldCurve.compute(ibRate)) else None
          val newBs = bs.copy(banks = afterResolve, interbankRate = Rate(ibRate), interbankCurve = curve)
          val reFirms =
            if anyFailed then
              in.rewiredFirms.map { f =>
                if f.bankId.toInt < afterResolve.length && afterResolve(f.bankId.toInt).failed then
                  f.copy(bankId = absorberId)
                else f
              }
            else in.rewiredFirms
          val reHouseholds =
            if anyFailed then
              in.finalHouseholds.map(_.map { h =>
                if h.bankId.toInt < afterResolve.length && afterResolve(h.bankId.toInt).failed then
                  h.copy(bankId = absorberId)
                else h
              })
            else in.finalHouseholds
          val aggBank = newBs.aggregate
          (Some(newBs), reFirms, reHouseholds, multiBailInLoss, multiCapDest)
        case None =>
          (None, in.rewiredFirms, in.finalHouseholds, 0.0, 0.0)

    val resolvedBank = finalBankingSector.map(_.aggregate).getOrElse(finalBank)

    val monAgg = if Config.CreditDiagnostics then
      val totalReserves = finalBankingSector.map(_.banks.kahanSumBy(_.reservesAtNbp.toDouble)).getOrElse(0.0)
      Some(MonetaryAggregates.compute(resolvedBank.deposits, PLN(totalReserves)))
    else None

    Output(
      resolvedBank,
      finalBankingSector,
      reassignedFirms,
      reassignedHouseholds,
      finalPpk,
      finalInsurance,
      finalNbfi,
      newGovWithYield,
      newJst,
      housingAfterFlows,
      bfgLevy,
      bailInLoss,
      multiCapDestruction,
      monAgg,
      finalHhAgg,
      vat,
      vatAfterEvasion,
      pitAfterEvasion,
      exciseRevenue,
      exciseAfterEvasion,
      customsDutyRevenue,
      effectiveShadowShare,
      mortgageInterestIncome,
      mortgagePrincipal,
      mortgageDefaultLoss,
      mortgageDefaultAmount,
      jstDepositChange,
      investNetDepositFlow,
      actualBondChange,
    )
