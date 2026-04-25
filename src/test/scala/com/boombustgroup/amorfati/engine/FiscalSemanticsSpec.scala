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
      unempBenefitSpend = PLN("40e6"),
      debtServiceSpend = PLN("30e6"),
      socialTransferSpend = PLN("50e6"),
      govCurrentSpend = PLN("200e6"),
      govCapitalSpend = PLN("80e6"),
      euProjectCapital = PLN("120e6"),
      euCofinancing = PLN("25e6"),
    )

    gov.domesticBudgetDemand shouldBe PLN("280e6")
  }

  "GovState.domesticBudgetOutlays" should "include domestic budget outlays but exclude total EU project envelope" in {
    val gov = FiscalBudget.GovState(
      taxRevenue = PLN.Zero,
      deficit = PLN.Zero,
      cumulativeDebt = PLN.Zero,
      unempBenefitSpend = PLN("40e6"),
      debtServiceSpend = PLN("30e6"),
      socialTransferSpend = PLN("50e6"),
      govCurrentSpend = PLN("200e6"),
      govCapitalSpend = PLN("80e6"),
      euProjectCapital = PLN("120e6"),
      euCofinancing = PLN("25e6"),
    )

    gov.domesticBudgetOutlays shouldBe PLN("425e6")
  }
