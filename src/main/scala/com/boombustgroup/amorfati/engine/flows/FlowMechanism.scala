package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.ledger.*

/** Mechanism IDs for audit trail. Each mechanism that emits flows gets a unique
  * ID. amor-fati specific — the ledger only sees MechanismId(Int).
  */
object FlowMechanism:
  val ZusContribution: MechanismId  = MechanismId(1)
  val ZusPension: MechanismId       = MechanismId(2)
  val ZusGovSubvention: MechanismId = MechanismId(3)
  val NfzContribution: MechanismId  = MechanismId(4)
  val NfzSpending: MechanismId      = MechanismId(5)
  val NfzGovSubvention: MechanismId = MechanismId(6)
