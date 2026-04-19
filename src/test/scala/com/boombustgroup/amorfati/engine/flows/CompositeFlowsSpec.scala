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
    val wage     = PLN(7000.0)
    val employed = 80000
    val retirees = 1000

    Vector.concat(
      // Tier 1: Social funds
      ZusFlows.emit(ZusFlows.ZusInput(employed, wage, retirees)),
      NfzFlows.emit(NfzFlows.NfzInput(employed, wage, 90000, retirees)),
      PpkFlows.emit(PpkFlows.PpkInput(employed, wage)),
      EarmarkedFlows.emit(EarmarkedFlows.Input(employed, wage, PLN(1000000.0), 10, 15)),
      JstFlows.emit(JstFlows.Input(PLN(5000000.0), PLN(50000000.0), PLN(100000000.0), 9000, PLN(3000000.0))),
      // Tier 2: Agents + Gov
      HouseholdFlows.emit(
        HouseholdFlows.Input(
          PLN(40000000.0),
          PLN(8000000.0),
          PLN(5000000.0),
          PLN(3000000.0),
          PLN(1000000.0),
          PLN(500000.0),
          PLN(2000000.0),
          PLN(1500000.0),
          PLN(200000.0),
        ),
      ),
      FirmFlows.emit(
        FirmFlows.Input(
          PLN(50000000.0),
          PLN(5000000.0),
          PLN(3000000.0),
          PLN(4000000.0),
          PLN(2000000.0),
          PLN(1500000.0),
          PLN(1000000.0),
          PLN(10000000.0),
          PLN(500000.0),
          PLN(300000.0),
          PLN(200000.0),
          PLN(6000000.0),
        ),
      ),
      GovBudgetFlows.emit(
        GovBudgetFlows.Input(
          vatRevenue = PLN(3000000.0),
          exciseRevenue = PLN(1200000.0),
          customsDutyRevenue = PLN(800000.0),
          govPurchases = PLN(2000000.0),
          debtService = PLN(500000.0),
          unempBenefitSpend = PLN(800000.0),
          socialTransferSpend = PLN(1200000.0),
          euCofinancing = PLN(300000.0),
          govCapitalSpend = PLN(400000.0),
        ),
      ),
      InsuranceFlows.emit(
        InsuranceFlows.Input(
          employed = employed,
          wage = wage,
          unempRate = Share(0.05),
          currentLifeReserves = PLN(90000000.0),
          currentNonLifeReserves = PLN(30000000.0),
          prevGovBondHoldings = PLN(50000000.0),
          prevCorpBondHoldings = PLN(20000000.0),
          corpBondDefaultLoss = PLN.Zero,
          prevEquityHoldings = PLN(10000000.0),
          govBondYield = Rate(0.06),
          corpBondYield = Rate(0.08),
          equityReturn = Rate(0.01),
        ),
      ),
      // Tier 3: Financial markets + external
      EquityFlows.emit(EquityFlows.Input(PLN(500000.0), PLN(200000.0), PLN(100000.0), PLN(50000.0), PLN(1000000.0))),
      CorpBondFlows.emit(CorpBondFlows.Input(PLN(300000.0), PLN(50000.0), PLN(1000000.0), PLN(200000.0))),
      MortgageFlows.emit(MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0))),
      OpenEconFlows.emit(
        OpenEconFlows.Input(
          PLN(20000000.0),
          PLN(18000000.0),
          PLN(1000000.0),
          PLN(800000.0),
          PLN(2000000.0),
          PLN(500000.0),
          PLN(-300000.0),
          PLN(1500000.0),
          PLN(400000.0),
          PLN(200000.0),
        ),
      ),
      BankingFlows.emit(
        BankingFlows.Input(
          firmInterestIncome = PLN(900000.0),
          firmNplLoss = PLN(120000.0),
          mortgageNplLoss = PLN(110000.0),
          consumerNplLoss = PLN(60000.0),
          govBondIncome = PLN(3000000.0),
          reserveInterest = PLN(500000.0),
          standingFacilityIncome = PLN(100000.0),
          interbankInterest = PLN(200000.0),
          corpBondCoupon = PLN(70000.0),
          corpBondDefaultLoss = PLN(50000.0),
          bfgLevy = PLN(400000.0),
          unrealizedBondLoss = PLN(150000.0),
          bailInLoss = PLN.Zero,
          nbpRemittance = PLN(800000.0),
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

  it should "preserve SFC across 120 months" in {
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { month =>
      balances = Interpreter.applyAll(balances, allFlows)
      withClue(s"SFC violated at month $month: ") {
        Interpreter.totalWealth(balances) shouldBe 0L
      }
    }
  }

  it should "have all mechanism IDs represented" in {
    val mechanisms = allFlows.map(_.mechanism).toSet
    mechanisms.size should be > 30
  }
