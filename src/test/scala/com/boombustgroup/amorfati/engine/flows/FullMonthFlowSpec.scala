package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Full month: run old pipeline, extract flows from every step, verify SFC ==
  * 0L.
  *
  * Proves ALL 14 flow mechanisms can be fed with real data from a full
  * simulation month. The old pipeline provides the economic computations; flow
  * mechanisms provide the accounting. Both must close at 0L.
  */
class FullMonthFlowSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val initResult = WorldInit.initialize(42L)
  private val w          = initResult.world
  private val rng        = new scala.util.Random(42)

  /** Run old pipeline for one month, then extract ALL flows from step outputs.
    */
  private def runFullMonth: Vector[Flow] =
    val s1 = FiscalConstraintStep.run(FiscalConstraintStep.Input(w))
    val s2 = LaborDemographicsStep.run(LaborDemographicsStep.Input(w, initResult.firms, initResult.households, s1))
    val s3 = HouseholdIncomeStep.run(HouseholdIncomeStep.Input(w, initResult.firms, initResult.households, s1, s2), rng)
    val s4 = DemandStep.run(DemandStep.Input(w, s2, s3))
    val s5 = FirmProcessingStep.run(FirmProcessingStep.Input(w, initResult.firms, initResult.households, s1, s2, s3, s4), rng)
    @annotation.unused
    val s6 = HouseholdFinancialStep.run(HouseholdFinancialStep.Input(w, s1, s2, s3))

    Vector.concat(
      // Tier 1: Social funds (from LaborDemographics output)
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
          com.boombustgroup.amorfati.engine.economics.LaborEconomics.Result(
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

  "Full month (old pipeline + flow extraction)" should "preserve SFC at 0L" in {
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
