package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Grand Unification Test: representative flow mechanisms fired together.
  *
  * Proves that the entire economy's monetary flows close at SFC == 0L when
  * composed through the verified interpreter. Uses realistic aggregate values
  * from a typical simulation month.
  */
class CompositeFlowsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private def allFlows: Vector[Flow] =
    val wage              = PLN(7000)
    val employed          = 80000
    val workingAge        = 90000
    val retirees          = 1000
    val govDebtRecipients = GovBudgetFlows.DebtServiceRecipients(
      banks = PLN(500000),
      foreign = PLN.Zero,
      nbp = PLN.Zero,
      insurance = PLN.Zero,
      ppk = PLN.Zero,
      tfi = PLN.Zero,
    )

    Vector.concat(
      // Tier 1: Social funds
      ZusFlows.emit(ZusFlows.ZusInput(employed, wage, retirees)),
      NfzFlows.emit(NfzFlows.NfzInput.fromDrivers(employed = employed, wage = wage, workingAge = workingAge, nRetirees = retirees)),
      PpkFlows.emit(PpkFlows.PpkInput(employed, wage)),
      EarmarkedFlows.emit(EarmarkedFlows.Input(employed, wage, PLN(1000000), 10, 15)),
      JstFlows.emit(JstFlows.Input(PLN(5000000), PLN(50000000), PLN(100000000), 9000, PLN(3000000))),
      // Tier 2: Agents + Gov
      HouseholdFlows.emit(
        HouseholdFlows.Input(
          PLN(40000000),
          PLN(8000000),
          PLN(5000000),
          PLN(3000000),
          PLN(1000000),
          PLN(500000),
          PLN(2000000),
          PLN(1500000),
          PLN(200000),
        ),
      ),
      FirmFlows.emit(
        FirmFlows.Input(
          PLN(50000000),
          PLN(5000000),
          PLN(3000000),
          PLN(4000000),
          PLN(2000000),
          PLN(1500000),
          PLN(1000000),
          PLN(10000000),
          PLN(500000),
          PLN(300000),
          PLN(200000),
          PLN(6000000),
        ),
      ),
      GovBudgetFlows.emit(
        GovBudgetFlows.Input(
          vatRevenue = PLN(3000000),
          exciseRevenue = PLN(1200000),
          customsDutyRevenue = PLN(800000),
          govCurrentSpend = PLN(2000000),
          debtService = PLN(500000),
          unempBenefitSpend = PLN(800000),
          socialTransferSpend = PLN(1200000),
          euCofinancing = PLN(300000),
          govCapitalSpend = PLN(400000),
          debtServiceRecipients = Some(govDebtRecipients),
        ),
      ),
      InsuranceFlows.emit(
        InsuranceFlows.Input(
          employed = employed,
          wage = wage,
          unempRate = Share.decimal(5, 2),
          currentLifeReserves = PLN(90000000),
          currentNonLifeReserves = PLN(30000000),
          prevGovBondHoldings = PLN(50000000),
          prevCorpBondHoldings = PLN(20000000),
          corpBondDefaultLoss = PLN.Zero,
          prevEquityHoldings = PLN(10000000),
          govBondYield = Rate.decimal(6, 2),
          corpBondYield = Rate.decimal(8, 2),
          equityReturn = Rate.decimal(1, 2),
        ),
      ),
      // Tier 3: Financial markets + external
      EquityFlows.emit(EquityFlows.Input(PLN(500000), PLN(200000), PLN(100000), PLN(50000))),
      CorpBondFlows.emit(CorpBondFlows.Input(PLN(300000), PLN(50000), PLN(1000000), PLN(200000))),
      MortgageFlows.emit(MortgageFlows.Input(PLN(5000000), PLN(2000000), PLN(1500000), PLN(300000))),
      OpenEconFlows.emit(
        OpenEconFlows.Input(
          exports = PLN(20000000),
          imports = PLN(18000000),
          tourismExport = PLN(1000000),
          tourismImport = PLN(800000),
          fdi = PLN(2000000),
          portfolioFlows = PLN(500000),
          carryTradeFlow = PLN(100000),
          primaryIncome = PLN(-300000),
          euFunds = PLN(1500000),
          diasporaInflow = PLN(400000),
          capitalFlightOutflow = PLN(200000),
        ),
      ),
      BankingFlows.emit(
        BankingFlows.Input(
          firmInterestIncome = PLN(900000),
          firmNplLoss = PLN(120000),
          mortgageNplLoss = PLN(110000),
          consumerNplLoss = PLN(60000),
          govBondIncome = PLN(3000000),
          reserveInterest = PLN(500000),
          standingFacilityIncome = PLN(100000),
          interbankInterest = PLN(200000),
          corpBondCoupon = PLN(70000),
          corpBondDefaultLoss = PLN(50000),
          bfgLevy = PLN(400000),
          unrealizedBondLoss = PLN(150000),
          bailInLoss = PLN.Zero,
          nbpRemittance = PLN(800000),
          fxReserveSettlement = PLN.Zero,
          standingFacilityBackstop = PLN.Zero,
        ),
      ),
    )

  "Composite flows" should "preserve total wealth at exactly 0L" in {
    val flows    = allFlows
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "produce non-trivial number of flows" in {
    allFlows.length should be > 40
  }

  it should "have all mechanism IDs represented" in {
    val mechanisms = allFlows.map(_.mechanism).toSet
    mechanisms.size should be > 30
  }
