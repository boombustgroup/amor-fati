package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Full month: run pipeline via Economics objects, extract flows from every
  * step, verify SFC == 0L.
  */
class FullMonthFlowSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val initResult = WorldInit.initialize(42L)
  private val w          = initResult.world
  private val rng        = new scala.util.Random(42)

  /** Run pipeline for one month using Economics objects. */
  private def runFullMonth: Vector[Flow] =
    val fiscal = FiscalConstraintEconomics.compute(w)
    val s1     = FiscalConstraintEconomics.toOutput(fiscal)
    val labor  = LaborEconomics.compute(w, initResult.firms, initResult.households, s1)
    val s2     = LaborEconomics.Output(
      labor.wage,
      labor.employed,
      labor.laborDemand,
      labor.wageGrowth,
      labor.immigration,
      labor.netMigration,
      labor.demographics,
      SocialSecurity.ZusState.zero,
      SocialSecurity.NfzState.zero,
      SocialSecurity.PpkState.zero,
      PLN.Zero,
      EarmarkedFunds.State.zero,
      labor.living,
      labor.regionalWages,
    )
    val s3     = HouseholdIncomeEconomics.compute(w, initResult.firms, initResult.households, s1.lendingBaseRate, s1.resWage, s2.newWage, rng)
    val s4     = DemandEconomics.compute(DemandEconomics.Input(w, s2.employed, s2.living, s3.domesticCons))
    val s5     = FirmEconomics.runStep(w, initResult.firms, initResult.households, s1, s2, s3, s4, rng)
    @annotation.unused
    val s6     = HouseholdFinancialEconomics.compute(w, s1.m, s2.employed, s3.hhAgg, rng)

    Vector.concat(
      // Tier 1: Social funds (from LaborEconomics output)
      ZusFlows.emit(ZusFlows.ZusInput(s2.employed, s2.newWage, s2.newDemographics.retirees)),
      NfzFlows.emit(NfzFlows.NfzInput(s2.employed, s2.newWage, s2.newDemographics.workingAgePop, s2.newDemographics.retirees)),
      PpkFlows.emit(PpkFlows.PpkInput(s2.employed, s2.newWage)),
      EarmarkedFlows.emit(
        EarmarkedFlows.Input(
          s2.employed,
          s2.newWage,
          PLN.Zero,
          initResult.firms.length - s2.living.length,
          if s2.living.nonEmpty then s2.laborDemand / s2.living.length else 0,
        ),
      ),
      JstFlows.emit(JstFlows.Input(w.gov.taxRevenue, s3.totalIncome, PLN(w.gdpProxy), s2.living.length, s3.pitRevenue)),
      // Tier 2: Agents
      HouseholdFlows.emit(StateAdapter.hhInput(s3.hhAgg)),
      FirmFlows.emit(
        FirmFlows.Input(
          wages = s3.totalIncome,
          cit = s5.sumTax,
          loanRepayment = s5.sumFirmPrincipal,
          newLoans = s5.sumNewLoans,
          interestPaid = s5.intIncome,
          capex = s5.sumCapex,
          equityIssuance = s5.sumEquityIssuance,
          bondIssuance = s5.sumBondIssuance,
          ioPayments = s5.totalIoPaid,
          nplDefault = s5.nplLoss,
          profitShifting = s5.sumProfitShifting,
          fdiRepatriation = s5.sumFdiRepatriation,
          grossInvestment = s5.sumGrossInvestment,
        ),
      ),
      GovBudgetFlows.emit(
        GovBudgetFlows.Input(
          taxRevenue = w.gov.taxRevenue,
          govPurchases = s4.govPurchases,
          debtService = w.gov.debtServiceSpend,
          unempBenefitSpend = s3.hhAgg.totalUnempBenefits,
          socialTransferSpend = s3.hhAgg.totalSocialTransfers,
          euCofinancing = PLN.Zero,
          govCapitalSpend = PLN.Zero,
        ),
      ),
      InsuranceFlows.emit(
        StateAdapter.insuranceInput(
          w,
          LaborEconomics.Result(
            s2.newWage,
            s2.employed,
            s2.laborDemand,
            s2.wageGrowth,
            s2.newDemographics,
            s2.newImmig,
            s2.netMigration,
            s2.living,
            s2.regionalWages,
            initResult.firms.length - s2.living.length,
            if s2.living.nonEmpty then s2.laborDemand / s2.living.length else 0,
          ),
        ),
      ),
      // Tier 3: Financial markets
      EquityFlows.emit(EquityFlows.Input(PLN.Zero, PLN.Zero, PLN.Zero, s5.sumEquityIssuance)),
      CorpBondFlows.emit(CorpBondFlows.Input(PLN.Zero, PLN.Zero, s5.actualBondIssuance, PLN.Zero)),
      MortgageFlows.emit(MortgageFlows.Input(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)),
      // OpenEcon: simplified
      OpenEconFlows.emit(
        OpenEconFlows.Input(
          w.forex.exports,
          w.forex.imports,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
        ),
      ),
      // Banking
      BankingFlows.emit(
        BankingFlows.Input(
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
        ),
      ),
    )

  "Full month (pipeline + flow extraction)" should "preserve SFC at 0L" in {
    val flows    = runFullMonth
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "produce flows from all tiers" in {
    val flows      = runFullMonth
    val mechanisms = flows.map(_.mechanism).toSet
    mechanisms.size should be > 15
  }

  it should "have non-trivial total flow volume" in {
    val flows       = runFullMonth
    val totalVolume = flows.map(_.amount).sum
    totalVolume should be > 0L
  }
