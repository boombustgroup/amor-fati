package sfc.engine.steps

import sfc.accounting.{BopState, ForexState}
import sfc.agents.*
import sfc.config.{RunConfig, SectorDefs, SimParams}
import sfc.engine.*
import sfc.engine.markets.{CorporateBondMarket, GvcTrade, OpenEconomy}
import sfc.engine.mechanisms.Expectations
import sfc.types.*
import sfc.util.KahanSum.*

object OpenEconomyStep:

  case class Input(
      w: World,
      rc: RunConfig,
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      s5: FirmProcessingStep.Output,
      s6: HouseholdFinancialStep.Output,
      s7: PriceEquityStep.Output,
  )

  case class Output(
      newForex: ForexState,
      newBop: BopState,
      newGvc: GvcTrade.State,
      newRefRate: Double,
      newExp: Expectations.State,
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
      corpBondAmort: Double,
      newInsurance: Insurance.State,
      insNetDepositChange: Double,
      newNbfi: Nbfi.State,
      nbfiDepositDrain: Double,
      oeValuationEffect: Double,
      fdiCitLoss: Double,
  )

  def run(in: Input)(using p: SimParams): Output =
    val living2 = in.s5.ioFirms.filter(Firm.isAlive)

    // Sector outputs for open economy
    val sectorOutputs = (0 until SectorDefs.length).map { s =>
      living2
        .filter(_.sector.toInt == s)
        .kahanSumBy(f => Firm.computeCapacity(f) * in.s4.sectorMults(f.sector.toInt) * in.w.priceLevel)
    }.toVector

    // GVC / Deep External Sector (v5.0)
    val newGvc =
      if p.flags.gvc && p.flags.openEcon then
        GvcTrade.step(in.w.external.gvc, sectorOutputs, in.w.priceLevel, in.w.forex.exchangeRate, in.s7.autoR, in.s1.m, in.rc)
      else in.w.external.gvc

    val (gvcExp, gvcImp) =
      if p.flags.gvc && p.flags.openEcon then (Some(newGvc.totalExports.toDouble), Some(newGvc.sectorImports.map(_.toDouble)))
      else (None, None)

    val totalTechAndInvImports                           = in.s5.sumTechImp + in.s7.investmentImports
    val (newForex, newBop0, oeValuationEffect, fxResult) = if p.flags.openEcon then
      val oeResult = OpenEconomy.step(
        in.w.bop,
        in.w.forex,
        in.s3.importCons,
        totalTechAndInvImports,
        in.s7.autoR,
        in.w.nbp.referenceRate.toDouble,
        in.s7.gdp,
        in.w.priceLevel,
        sectorOutputs,
        in.s1.m,
        in.rc,
        nbpFxReserves = in.w.nbp.fxReserves.toDouble,
        gvcExports = gvcExp,
        gvcIntermImports = gvcImp,
        remittanceOutflow = in.s6.remittanceOutflow,
        euFundsMonthly = in.s7.euMonthly,
        diasporaInflow = in.s6.diasporaInflow,
        tourismExport = in.s6.tourismExport,
        tourismImport = in.s6.tourismImport,
      )
      (oeResult.forex, oeResult.bop, oeResult.valuationEffect, oeResult.fxIntervention)
    else
      val fx = OpenEconomy.updateForeign(
        in.w.forex,
        in.s3.importCons,
        totalTechAndInvImports,
        in.s7.autoR,
        in.w.nbp.referenceRate.toDouble,
        in.s7.gdp,
        in.rc,
      )
      (fx, in.w.bop, 0.0, Nbp.FxInterventionResult(0.0, 0.0, in.w.nbp.fxReserves.toDouble))

    // Adjust BOP for foreign dividend outflow (primary income component) + EU funds tracking
    val newBop1          =
      if in.s7.foreignDividendOutflow > 0 && p.flags.openEcon then
        newBop0.copy(
          currentAccount = newBop0.currentAccount - PLN(in.s7.foreignDividendOutflow),
          nfa = newBop0.nfa - PLN(in.s7.foreignDividendOutflow),
        )
      else newBop0
    // FDI composition (#33): profit shifting (service import) + repatriation (primary income debit)
    val fdiTotalBopDebit = in.s5.sumProfitShifting + in.s5.sumFdiRepatriation
    val newBop2          =
      if fdiTotalBopDebit > 0 && p.flags.fdi && p.flags.openEcon then
        newBop1.copy(
          currentAccount = newBop1.currentAccount - PLN(fdiTotalBopDebit),
          nfa = newBop1.nfa - PLN(fdiTotalBopDebit),
          tradeBalance = newBop1.tradeBalance - PLN(in.s5.sumProfitShifting),
          totalImports = newBop1.totalImports + PLN(in.s5.sumProfitShifting),
        )
      else newBop1
    val fdiCitLoss       = in.s5.sumProfitShifting * p.fiscal.citRate.toDouble
    val newBop           = newBop2.copy(
      euFundsMonthly = PLN(in.s7.euMonthly),
      euCumulativeAbsorption = in.w.bop.euCumulativeAbsorption + PLN(in.s7.euMonthly),
    )

    val exRateChg  = (newForex.exchangeRate / in.w.forex.exchangeRate) - 1.0
    val newRefRate =
      Nbp.updateRate(
        in.w.nbp.referenceRate.toDouble,
        in.s7.newInfl,
        exRateChg,
        in.s2.employed,
        in.w.totalPopulation,
        in.rc,
      )

    // Expectations step: update after inflation + rate computed
    val unempRateForExp = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val newExp          =
      if p.flags.expectations then Expectations.step(in.w.mechanisms.expectations, in.s7.newInfl, newRefRate, unempRateForExp, in.rc)
      else in.w.mechanisms.expectations

    // Reserve interest, standing facilities, interbank interest
    val bsec                             = in.w.bankingSector
    val (_, totalReserveInterest)        = Banking.computeReserveInterest(bsec.banks, in.w.nbp.referenceRate.toDouble)
    val (_, totalStandingFacilityIncome) =
      Banking.computeStandingFacilities(bsec.banks, in.w.nbp.referenceRate.toDouble)
    val (_, totalInterbankInterest)      = Banking.interbankInterestFlows(bsec.banks, bsec.interbankRate.toDouble)

    // --- Bond market + QE ---
    val annualGdpForBonds = in.w.gdpProxy * 12.0
    val debtToGdp         = if annualGdpForBonds > 0 then in.w.gov.cumulativeDebt.toDouble / annualGdpForBonds else 0.0
    val nbpBondGdpShare   = if annualGdpForBonds > 0 then in.w.nbp.qeCumulative.toDouble / annualGdpForBonds else 0.0
    // Channel 3: De-anchored expectations → higher bond yields
    val credPremium       = if p.flags.expectations then
      val target = p.monetary.targetInfl.toDouble
      (1.0 - in.w.mechanisms.expectations.credibility.toDouble) *
        Math.abs(in.w.mechanisms.expectations.expectedInflation.toDouble - target) *
        p.labor.expBondSensitivity.toDouble
    else 0.0
    val newBondYield      = Nbp.bondYield(newRefRate, debtToGdp, nbpBondGdpShare, in.w.bop.nfa.toDouble, credPremium)

    // Debt service: use LAGGED bond stock
    val rawDebtService     = in.w.gov.bondsOutstanding.toDouble * newBondYield / 12.0
    val monthlyDebtService = Math.min(rawDebtService, in.w.gdpProxy * 0.50)
    val bankBondIncome     = in.w.bank.govBondHoldings.toDouble * newBondYield / 12.0
    val nbpBondIncome      = in.w.nbp.govBondHoldings.toDouble * newBondYield / 12.0
    val nbpRemittance      = nbpBondIncome - totalReserveInterest - totalStandingFacilityIncome

    // QE logic
    val qeActivate                    = Nbp.shouldActivateQe(newRefRate, in.s7.newInfl)
    val qeTaper                       = Nbp.shouldTaperQe(in.s7.newInfl)
    val qeActive                      =
      if qeActivate then true
      else if qeTaper then false
      else in.w.nbp.qeActive
    val preQeNbp                      = Nbp.State(Rate(newRefRate), in.w.nbp.govBondHoldings, qeActive, in.w.nbp.qeCumulative)
    val (postQeNbp, qePurchaseAmount) = Nbp.executeQe(preQeNbp, in.w.bank.govBondHoldings.toDouble, annualGdpForBonds)
    val postFxNbp                     = postQeNbp.copy(
      fxReserves = PLN(fxResult.newReserves),
      lastFxTraded = PLN(fxResult.eurTraded),
    )

    // --- Corporate bond market step (#40) ---
    val corpBondAmort                      = CorporateBondMarket.amortization(in.w.financial.corporateBonds)
    val newCorpBonds                       = CorporateBondMarket
      .step(in.w.financial.corporateBonds, newBondYield, in.w.bank.nplRatio, in.s5.totalBondDefault, in.s5.actualBondIssuance)
      .copy(lastAbsorptionRate = Ratio(in.s5.corpBondAbsorption))
    val (_, corpBondBankCoupon, _)         = CorporateBondMarket.computeCoupon(in.w.financial.corporateBonds)
    val (_, _, corpBondBankDefaultLoss, _) =
      CorporateBondMarket.processDefaults(in.w.financial.corporateBonds, in.s5.totalBondDefault)

    // --- Insurance sector step ---
    val insUnempRate        = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val newInsurance        =
      if p.flags.insurance then
        Insurance.step(
          in.w.financial.insurance,
          in.s2.employed,
          in.s2.newWage,
          in.w.priceLevel,
          insUnempRate,
          newBondYield,
          in.w.financial.corporateBonds.corpBondYield.toDouble,
          in.w.financial.equity.monthlyReturn.toDouble,
        )
      else in.w.financial.insurance
    val insNetDepositChange = newInsurance.lastNetDepositChange.toDouble

    // --- Shadow Banking / NBFI step ---
    val nbfiDepositRate  = Math.max(0.0, postFxNbp.referenceRate.toDouble - 0.02)
    val nbfiUnempRate    = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val newNbfi          =
      if p.flags.nbfi then
        Nbfi.step(
          in.w.financial.nbfi,
          in.s2.employed,
          in.s2.newWage,
          in.w.priceLevel,
          nbfiUnempRate,
          in.w.bank.nplRatio,
          newBondYield,
          in.w.financial.corporateBonds.corpBondYield.toDouble,
          in.w.financial.equity.monthlyReturn.toDouble,
          nbfiDepositRate,
          in.s3.domesticCons,
        )
      else in.w.financial.nbfi
    val nbfiDepositDrain = newNbfi.lastDepositDrain.toDouble

    Output(
      newForex,
      newBop,
      newGvc,
      newRefRate,
      newExp,
      totalReserveInterest,
      totalStandingFacilityIncome,
      totalInterbankInterest,
      newBondYield,
      monthlyDebtService,
      bankBondIncome,
      nbpRemittance,
      postFxNbp,
      qePurchaseAmount,
      newCorpBonds,
      corpBondBankCoupon,
      corpBondBankDefaultLoss,
      corpBondAmort,
      newInsurance,
      insNetDepositChange,
      newNbfi,
      nbfiDepositDrain,
      oeValuationEffect,
      fdiCitLoss,
    )
