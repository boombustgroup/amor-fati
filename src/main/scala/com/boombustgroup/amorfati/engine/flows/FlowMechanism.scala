package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.ledger.*

/** Mechanism IDs for audit trail. Each mechanism that emits flows gets a unique
  * ID. amor-fati specific — the ledger only sees MechanismId(Int).
  */
object FlowMechanism:
  val ZusContribution: MechanismId      = MechanismId(1)
  val ZusPension: MechanismId           = MechanismId(2)
  val ZusGovSubvention: MechanismId     = MechanismId(3)
  val NfzContribution: MechanismId      = MechanismId(4)
  val NfzSpending: MechanismId          = MechanismId(5)
  val NfzGovSubvention: MechanismId     = MechanismId(6)
  val PpkContribution: MechanismId      = MechanismId(7)
  val PpkBondPurchase: MechanismId      = MechanismId(8)
  // Earmarked funds
  val FpContribution: MechanismId       = MechanismId(9)
  val FpSpending: MechanismId           = MechanismId(10)
  val FpGovSubvention: MechanismId      = MechanismId(11)
  val PfronContribution: MechanismId    = MechanismId(12)
  val PfronSpending: MechanismId        = MechanismId(13)
  val PfronGovSubvention: MechanismId   = MechanismId(14)
  val FgspContribution: MechanismId     = MechanismId(15)
  val FgspSpending: MechanismId         = MechanismId(16)
  val FgspGovSubvention: MechanismId    = MechanismId(17)
  // JST
  val JstRevenue: MechanismId           = MechanismId(18)
  val JstSpending: MechanismId          = MechanismId(19)
  // Government budget
  val GovTaxRevenue: MechanismId        = MechanismId(20)
  val GovPurchases: MechanismId         = MechanismId(21)
  val GovDebtService: MechanismId       = MechanismId(22)
  val GovCapitalInvestment: MechanismId = MechanismId(23)
  val GovUnempBenefit: MechanismId      = MechanismId(24)
  val GovSocialTransfer: MechanismId    = MechanismId(25)
  val GovEuCofin: MechanismId           = MechanismId(26)
  // Insurance
  val InsLifePremium: MechanismId       = MechanismId(27)
  val InsNonLifePremium: MechanismId    = MechanismId(28)
  val InsLifeClaim: MechanismId         = MechanismId(29)
  val InsNonLifeClaim: MechanismId      = MechanismId(30)
  val InsInvestmentIncome: MechanismId  = MechanismId(31)
