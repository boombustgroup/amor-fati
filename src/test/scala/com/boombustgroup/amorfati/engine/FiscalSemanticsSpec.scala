package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.engine.markets.FiscalBudget
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FiscalSemanticsSpec extends AnyFlatSpec with Matchers:

  "GovState.domesticBudgetDemand" should "track only domestic current and domestic capital spending" in {
    val gov = FiscalBudget.GovState(
      taxRevenue = PLN.Zero,
      deficit = PLN.Zero,
      cumulativeDebt = PLN.Zero,
      unempBenefitSpend = PLN(40000000),
      debtServiceSpend = PLN(30000000),
      socialTransferSpend = PLN(50000000),
      govCurrentSpend = PLN(200000000),
      govCapitalSpend = PLN(80000000),
      euProjectCapital = PLN(120000000),
      euCofinancing = PLN(25000000),
    )

    gov.domesticBudgetDemand shouldBe PLN(280000000)
  }

  "GovState.domesticBudgetOutlays" should "include domestic budget outlays but exclude total EU project envelope" in {
    val gov = FiscalBudget.GovState(
      taxRevenue = PLN.Zero,
      deficit = PLN.Zero,
      cumulativeDebt = PLN.Zero,
      unempBenefitSpend = PLN(40000000),
      debtServiceSpend = PLN(30000000),
      socialTransferSpend = PLN(50000000),
      govCurrentSpend = PLN(200000000),
      govCapitalSpend = PLN(80000000),
      euProjectCapital = PLN(120000000),
      euCofinancing = PLN(25000000),
    )

    gov.domesticBudgetOutlays shouldBe PLN(425000000)
  }
