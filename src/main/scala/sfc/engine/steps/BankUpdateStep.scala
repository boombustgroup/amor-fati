package sfc.engine.steps

import sfc.accounting.{BankingAggregate, BopState, GovState, MonetaryAggregates}
import sfc.agents.*
import sfc.McRunConfig
import sfc.config.SimParams
import sfc.engine.*
import sfc.engine.markets.{FiscalBudget, HousingMarket}
import sfc.engine.mechanisms.YieldCurve
import sfc.types.*
import sfc.util.KahanSum.*

object BankUpdateStep:

  case class Input(
      w: World,
      rc: McRunConfig,
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      s5: FirmProcessingStep.Output,
      s6: HouseholdFinancialStep.Output,
      s7: PriceEquityStep.Output,
      s8: OpenEconomyStep.Output,
  )

  case class Output(
      resolvedBank: BankingAggregate,
      finalBankingSector: Banking.State,
      reassignedFirms: Vector[Firm.State],
      reassignedHouseholds: Vector[Household.State],
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
      finalHhAgg: Household.Aggregates,
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

  def run(in: Input)(using p: SimParams): Output =
    val vat                 = in.s3.consumption * p.fiscal.fofConsWeights
      .map(_.toDouble)
      .zip(p.fiscal.vatRates.map(_.toDouble))
      .map((w, r) => w * r)
      .kahanSum
    val exciseRevenue       = in.s3.consumption * p.fiscal.fofConsWeights
      .map(_.toDouble)
      .zip(p.fiscal.exciseRates.map(_.toDouble))
      .map((w, r) => w * r)
      .kahanSum
    val customsDutyRevenue  =
      if p.flags.openEcon then in.s8.newBop.totalImports.toDouble * p.fiscal.customsNonEuShare.toDouble * p.fiscal.customsDutyRate.toDouble
      else 0.0
    val unempBenefitSpend   = in.s3.hhAgg.totalUnempBenefits.toDouble
    val socialTransferSpend =
      if p.flags.social800 then in.s3.hhAgg.totalSocialTransfers.toDouble
      else 0.0

    // Informal economy: aggregate tax evasion
    val informalCyclicalAdj  = in.w.mechanisms.informalCyclicalAdj
    val effectiveShadowShare =
      if p.flags.informal then
        p.fiscal.fofConsWeights
          .map(_.toDouble)
          .zip(p.informal.sectorShares.map(_.toDouble))
          .map((cw, ss) => cw * Math.min(1.0, ss + informalCyclicalAdj))
          .kahanSum
      else 0.0
    val vatAfterEvasion      =
      if p.flags.informal then vat * (1.0 - effectiveShadowShare * p.informal.vatEvasion.toDouble) else vat
    val exciseAfterEvasion   =
      if p.flags.informal then exciseRevenue * (1.0 - effectiveShadowShare * p.informal.exciseEvasion.toDouble)
      else exciseRevenue
    val pitAfterEvasion      =
      if p.flags.informal then in.s3.pitRevenue * (1.0 - effectiveShadowShare * p.informal.pitEvasion.toDouble)
      else in.s3.pitRevenue

    val newGov          = FiscalBudget.update(
      in.w.gov,
      in.s5.sumTax + in.s7.dividendTax + pitAfterEvasion,
      vatAfterEvasion,
      in.s7.newPrice,
      unempBenefitSpend,
      in.s8.monthlyDebtService,
      in.s8.nbpRemittance,
      in.s2.newZus.govSubvention.toDouble,
      socialTransferSpend,
      euCofinancing = in.s7.euCofin,
      euProjectCapital = in.s7.euProjectCapital,
      exciseRevenue = exciseAfterEvasion,
      customsDutyRevenue = customsDutyRevenue,
      govPurchasesActual = in.s4.govPurchases,
    )
    val newGovWithYield = newGov.copy(bondYield = Rate(in.s8.newBondYield))

    // JST (local government)
    val nLivingFirms               = in.s5.ioFirms.count(Firm.isAlive)
    val (newJst, jstDepositChange) =
      Jst.step(
        in.w.social.jst,
        newGovWithYield.taxRevenue.toDouble,
        in.s3.totalIncome,
        in.s7.gdp,
        nLivingFirms,
        pitAfterEvasion,
      )

    // ---- Housing market step ----
    val unempRate                = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val prevMortgageRate         = in.w.real.housing.avgMortgageRate
    val mortgageBaseRate: Double =
      if p.flags.interbankTermStructure then YieldCurve.compute(in.w.bankingSector.interbankRate.toDouble).wibor3m.toDouble
      else in.w.nbp.referenceRate.toDouble
    val mortgageRate             = mortgageBaseRate + p.housing.mortgageSpread.toDouble

    val housingAfterPrice                                                =
      HousingMarket.step(
        in.w.real.housing,
        mortgageRate,
        in.s7.newInfl,
        in.s2.wageGrowth,
        in.s2.employed,
        prevMortgageRate.toDouble,
      )
    val housingAfterOrig                                                 = HousingMarket.processOrigination(housingAfterPrice, in.s3.totalIncome, mortgageRate, true)
    val (mortgageInterestIncome, mortgagePrincipal, mortgageDefaultLoss) =
      HousingMarket.processMortgageFlows(housingAfterOrig, mortgageRate, unempRate)
    val mortgageDefaultAmount                                            =
      if p.housing.mortgageRecovery.toDouble < 1.0 then mortgageDefaultLoss / (1.0 - p.housing.mortgageRecovery.toDouble)
      else 0.0
    val housingAfterFlows                                                =
      HousingMarket.applyFlows(housingAfterOrig, mortgagePrincipal, mortgageDefaultAmount, mortgageInterestIncome)

    // BFG levy (#48)
    val bfgLevy =
      if p.flags.bankFailure then
        val (_, total) = Banking.computeBfgLevy(in.w.bankingSector.banks)
        total
      else 0.0

    // Investment net deposit flow
    val currentInvestDomestic = in.s5.sumGrossInvestment * (1.0 - p.capital.importShare.toDouble) +
      in.s5.sumGreenInvestment * (1.0 - p.climate.greenImportShare.toDouble)
    val investNetDepositFlow  = in.s4.laggedInvestDemand - currentInvestDomestic

    val newBank = in.w.bank.copy(
      totalLoans = PLN(
        Math.max(0, in.w.bank.totalLoans.toDouble + in.s5.sumNewLoans - in.s5.nplNew * p.banking.loanRecovery.toDouble),
      ),
      nplAmount = PLN(Math.max(0, in.w.bank.nplAmount.toDouble + in.s5.nplNew - in.w.bank.nplAmount.toDouble * 0.05)),
      capital = PLN(
        in.w.bank.capital.toDouble - in.s5.nplLoss - mortgageDefaultLoss - in.s6.consumerNplLoss
          - in.s8.corpBondBankDefaultLoss - bfgLevy
          + in.s5.intIncome * p.banking.profitRetention.toDouble + in.s6.hhDebtService * p.banking.profitRetention.toDouble
          + in.s8.bankBondIncome * p.banking.profitRetention.toDouble - in.s6.depositInterestPaid * p.banking.profitRetention.toDouble
          + in.s8.totalReserveInterest * p.banking.profitRetention.toDouble + in.s8.totalStandingFacilityIncome * p.banking.profitRetention.toDouble
          + in.s8.totalInterbankInterest * p.banking.profitRetention.toDouble
          + mortgageInterestIncome * p.banking.profitRetention.toDouble
          + in.s6.consumerDebtService * p.banking.profitRetention.toDouble
          + in.s8.corpBondBankCoupon * p.banking.profitRetention.toDouble,
      ),
      deposits = PLN(
        in.w.bank.deposits.toDouble + (in.s3.totalIncome - in.s3.consumption) + investNetDepositFlow
          + jstDepositChange
          + in.s7.netDomesticDividends - in.s7.foreignDividendOutflow - in.s6.remittanceOutflow + in.s6.diasporaInflow
          + in.s6.tourismExport - in.s6.tourismImport
          + in.s6.consumerOrigination + in.s8.insNetDepositChange + in.s8.nbfiDepositDrain,
      ),
      consumerLoans = PLN(
        Math.max(
          0.0,
          in.w.bank.consumerLoans.toDouble + in.s6.consumerOrigination - in.s6.consumerPrincipal - in.s6.consumerDefaultAmt,
        ),
      ),
      consumerNpl = PLN(
        Math.max(0.0, in.w.bank.consumerNpl.toDouble + in.s6.consumerDefaultAmt - in.w.bank.consumerNpl.toDouble * 0.05),
      ),
      corpBondHoldings = in.s8.newCorpBonds.bankHoldings,
    )

    // Recompute hhAgg from final households
    val monthlyRetAttempts  = in.s3.hhAgg.retrainingAttempts
    val monthlyRetSuccesses = in.s3.hhAgg.retrainingSuccesses
    val finalHhAgg          = Household.computeAggregates(
      in.s5.households,
      PLN(in.s2.newWage),
      PLN(in.s1.resWage),
      in.s3.importAdj,
      monthlyRetAttempts,
      monthlyRetSuccesses,
    )

    val actualBondChange = (newGovWithYield.bondsOutstanding - in.w.gov.bondsOutstanding).toDouble

    // PPK bond purchases (capped at available bonds after QE)
    val availableBondsForPpk = newBank.govBondHoldings.toDouble +
      (if p.flags.govBondMarket then actualBondChange else 0.0) - in.s8.qePurchaseAmount
    val ppkBondPurchase      = Math.min(in.s2.rawPpkBondPurchase, Math.max(0.0, availableBondsForPpk))
    val finalPpk             = in.s2.newPpk.copy(bondHoldings = PLN(in.w.social.ppk.bondHoldings.toDouble + ppkBondPurchase))

    // Insurance gov bond purchases (capped at available bonds after QE + PPK)
    val insGovBondDelta      = (in.s8.newInsurance.govBondHoldings - in.w.financial.insurance.govBondHoldings).toDouble
    val availableBondsForIns = newBank.govBondHoldings.toDouble +
      (if p.flags.govBondMarket then actualBondChange else 0.0) - in.s8.qePurchaseAmount - ppkBondPurchase
    val insBondPurchase      = Math.max(0.0, Math.min(Math.max(0.0, insGovBondDelta), Math.max(0.0, availableBondsForIns)))
    val finalInsurance       =
      in.s8.newInsurance.copy(govBondHoldings = PLN(in.w.financial.insurance.govBondHoldings.toDouble + insBondPurchase))

    // TFI gov bond purchases (#42)
    val tfiGovBondDelta      = (in.s8.newNbfi.tfiGovBondHoldings - in.w.financial.nbfi.tfiGovBondHoldings).toDouble
    val availableBondsForTfi = newBank.govBondHoldings.toDouble +
      (if p.flags.govBondMarket then actualBondChange else 0.0) -
      in.s8.qePurchaseAmount - ppkBondPurchase - insBondPurchase
    val tfiBondPurchase      = Math.max(0.0, Math.min(Math.max(0.0, tfiGovBondDelta), Math.max(0.0, availableBondsForTfi)))
    val finalNbfi            =
      in.s8.newNbfi.copy(tfiGovBondHoldings = PLN(in.w.financial.nbfi.tfiGovBondHoldings.toDouble + tfiBondPurchase))

    // ---- Multi-bank update path ----
    val bs                       = in.w.bankingSector
    val (perBankReserveInt, _)   = Banking.computeReserveInterest(bs.banks, in.w.nbp.referenceRate.toDouble)
    val (perBankStandingFac, _)  = Banking.computeStandingFacilities(bs.banks, in.w.nbp.referenceRate.toDouble)
    val (perBankInterbankInt, _) = Banking.interbankInterestFlows(bs.banks, bs.interbankRate.toDouble)

    val totalWorkers                   = in.s5.perBankWorkers.kahanSumBy(_.toDouble)
    val updatedBanks                   = bs.banks.map { b =>
      val bId                     = b.id.toInt
      val bankNplNew              = in.s5.perBankNplDebt(bId)
      val bankNplLoss             = bankNplNew * (1.0 - p.banking.loanRecovery.toDouble)
      val bankIntIncome           = in.s5.perBankIntIncome(bId)
      val (
        bankIncomeShare,
        bankConsShare,
        bankHhDebtService,
        bankDepInterest,
        bankCcDSvc,
        bankCcOrig,
        bankCcDef,
      )                           =
        in.s3.perBankHhFlowsOpt match
          case Some(pbf) =>
            val f = pbf(bId)
            (
              f.income.toDouble,
              f.consumption.toDouble,
              f.debtService.toDouble,
              f.depositInterest.toDouble,
              f.consumerDebtService.toDouble,
              f.consumerOrigination.toDouble,
              f.consumerDefault.toDouble,
            )
          case None      =>
            val ws = if totalWorkers > 0 then in.s5.perBankWorkers(bId) / totalWorkers else 0.0
            (
              in.s3.totalIncome * ws,
              in.s3.consumption * ws,
              in.s6.hhDebtService * ws,
              0.0,
              in.s6.consumerDebtService * ws,
              in.s6.consumerOrigination * ws,
              in.s6.consumerDefaultAmt * ws,
            )
      val bankBondInc             = b.govBondHoldings.toDouble * in.s8.newBondYield / 12.0
      val bankResInt              = perBankReserveInt(bId)
      val bankSfInc               = perBankStandingFac(bId)
      val bankIbInt               = perBankInterbankInt(bId)
      val newLoansTotal           =
        Math.max(0.0, b.loans.toDouble + in.s5.perBankNewLoans(bId) - bankNplNew * p.banking.loanRecovery.toDouble)
      val ws                      = if totalWorkers > 0 then in.s5.perBankWorkers(bId) / totalWorkers else 0.0
      val bankDivInflow           = in.s7.netDomesticDividends * ws
      val bankDivOutflow          = in.s7.foreignDividendOutflow * ws
      val bankRemittance          = in.s6.remittanceOutflow * ws
      val bankDiasporaInflow      = in.s6.diasporaInflow * ws
      val bankTourismExport       = in.s6.tourismExport * ws
      val bankTourismImport       = in.s6.tourismImport * ws
      val bankInsDepChange        = in.s8.insNetDepositChange * ws
      val bankNbfiDepDrain        = in.s8.nbfiDepositDrain * ws
      val bankJstDepChange        = jstDepositChange * ws
      val bankInvestNetFlow       = investNetDepositFlow * ws
      val newDep                  =
        b.deposits.toDouble + (bankIncomeShare - bankConsShare) + bankInvestNetFlow + bankJstDepChange + bankDivInflow - bankDivOutflow - bankRemittance + bankDiasporaInflow + bankTourismExport - bankTourismImport + bankCcOrig + bankInsDepChange + bankNbfiDepDrain
      val bankDepShare            = if totalWorkers > 0 then in.s5.perBankWorkers(bId) / totalWorkers else 0.0
      val bankMortgageIntIncome   = mortgageInterestIncome * bankDepShare
      val bankMortgageNplLoss     = mortgageDefaultLoss * bankDepShare
      val bankCcNplLoss           = bankCcDef * (1.0 - p.household.ccNplRecovery.toDouble)
      val bankCcPrincipal         = in.s3.perBankHhFlowsOpt match
        case Some(pbf) => pbf(bId).consumerPrincipal.toDouble
        case _         =>
          if p.household.ccAmortRate.toDouble + (in.s1.lendingBaseRate + p.household.ccSpread.toDouble) / 12.0 > 0 then
            bankCcDSvc * (p.household.ccAmortRate.toDouble / (p.household.ccAmortRate.toDouble + (in.s1.lendingBaseRate + p.household.ccSpread.toDouble) / 12.0))
          else 0.0
      val bankCorpBondCoupon      = in.s8.corpBondBankCoupon * bankDepShare
      val bankCorpBondDefaultLoss = in.s8.corpBondBankDefaultLoss * bankDepShare
      val bankBfgLevy             =
        if p.flags.bankFailure && !b.failed then b.deposits.toDouble * p.banking.bfgLevyRate.toDouble / 12.0
        else 0.0
      b.copy(
        loans = PLN(newLoansTotal),
        nplAmount = PLN(Math.max(0.0, b.nplAmount.toDouble + bankNplNew - b.nplAmount.toDouble * 0.05)),
        capital = PLN(
          b.capital.toDouble - bankNplLoss - bankMortgageNplLoss - bankCcNplLoss
            - bankCorpBondDefaultLoss - bankBfgLevy + bankIntIncome * p.banking.profitRetention.toDouble +
            bankHhDebtService * p.banking.profitRetention.toDouble + bankBondInc * p.banking.profitRetention.toDouble - bankDepInterest * p.banking.profitRetention.toDouble
            + bankResInt * p.banking.profitRetention.toDouble + bankSfInc * p.banking.profitRetention.toDouble + bankIbInt * p.banking.profitRetention.toDouble
            + bankMortgageIntIncome * p.banking.profitRetention.toDouble
            + bankCcDSvc * p.banking.profitRetention.toDouble
            + bankCorpBondCoupon * p.banking.profitRetention.toDouble,
        ),
        deposits = PLN(newDep),
        demandDeposits = PLN(newDep * (1.0 - p.banking.termDepositFrac.toDouble)),
        termDeposits = PLN(newDep * p.banking.termDepositFrac.toDouble),
        loansShort = PLN(newLoansTotal * 0.20),
        loansMedium = PLN(newLoansTotal * 0.30),
        loansLong = PLN(newLoansTotal * 0.50),
        consumerLoans = PLN(Math.max(0.0, b.consumerLoans.toDouble + bankCcOrig - bankCcPrincipal - bankCcDef)),
        consumerNpl = PLN(Math.max(0.0, b.consumerNpl.toDouble + bankCcDef - b.consumerNpl.toDouble * 0.05)),
        corpBondHoldings = in.s8.newCorpBonds.bankHoldings * bankDepShare,
      )
    }
    val ibRate                         = Banking.interbankRate(updatedBanks, in.w.nbp.referenceRate.toDouble)
    val afterInterbank                 = Banking.clearInterbank(updatedBanks, bs.configs, ibRate)
    val afterBonds                     =
      if p.flags.govBondMarket then Banking.allocateBonds(afterInterbank, actualBondChange)
      else afterInterbank
    val afterQe                        = Banking.allocateQePurchases(afterBonds, in.s8.qePurchaseAmount)
    val afterPpk                       = Banking.allocateQePurchases(afterQe, ppkBondPurchase)
    val afterIns                       = Banking.allocateQePurchases(afterPpk, insBondPurchase)
    val afterTfi                       = Banking.allocateQePurchases(afterIns, tfiBondPurchase)
    val (afterFailCheck, anyFailed)    =
      Banking.checkFailures(afterTfi, in.s1.m, p.flags.bankFailure, in.s7.newMacropru.ccyb.toDouble)
    val (afterBailIn, multiBailInLoss) =
      if anyFailed then Banking.applyBailIn(afterFailCheck) else (afterFailCheck, 0.0)
    val (afterResolve, rawAbsorberId)  =
      if anyFailed then Banking.resolveFailures(afterBailIn)
      else (afterBailIn, BankId.NoBank)
    val absorberId                     =
      if rawAbsorberId.toInt >= 0 then rawAbsorberId
      else Banking.healthiestBankId(afterResolve)
    val multiCapDest                   =
      if anyFailed then
        afterTfi
          .zip(afterFailCheck)
          .map { (pre, post) =>
            if !pre.failed && post.failed then pre.capital.toDouble else 0.0
          }
          .kahanSum
      else 0.0
    val curve                          = if p.flags.interbankTermStructure then Some(YieldCurve.compute(ibRate)) else None
    val finalBankingSector             = bs.copy(banks = afterResolve, interbankRate = Rate(ibRate), interbankCurve = curve)
    val reassignedFirms                =
      if anyFailed then
        in.s7.rewiredFirms.map: f =>
          if f.bankId.toInt < afterResolve.length && afterResolve(f.bankId.toInt).failed then f.copy(bankId = absorberId)
          else f
      else in.s7.rewiredFirms
    val reassignedHouseholds           =
      if anyFailed then
        in.s5.households.map: h =>
          if h.bankId.toInt < afterResolve.length && afterResolve(h.bankId.toInt).failed then h.copy(bankId = absorberId)
          else h
      else in.s5.households
    val bailInLoss                     = multiBailInLoss
    val multiCapDestruction            = multiCapDest

    val resolvedBank = finalBankingSector.aggregate

    val monAgg = if p.flags.creditDiagnostics then
      val totalReserves = finalBankingSector.banks.kahanSumBy(_.reservesAtNbp.toDouble)
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
