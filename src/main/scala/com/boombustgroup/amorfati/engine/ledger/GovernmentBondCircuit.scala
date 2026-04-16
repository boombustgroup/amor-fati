package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.types.PLN

/** Government-bond issuer/holder stock view used by SFC level checks. */
case class GovernmentBondCircuit(
    outstanding: PLN,
    bankHoldings: PLN,
    foreignHoldings: PLN,
    nbpHoldings: PLN,
    insuranceHoldings: PLN,
    ppkHoldings: PLN,
    tfiHoldings: PLN,
):
  def totalHoldings: PLN =
    bankHoldings + foreignHoldings + nbpHoldings + insuranceHoldings + ppkHoldings + tfiHoldings

object GovernmentBondCircuit:
  def from(world: World, banks: Vector[Banking.BankState]): GovernmentBondCircuit =
    val bankAgg = Banking.aggregateFromBanks(banks)
    GovernmentBondCircuit(
      outstanding = world.gov.bondsOutstanding,
      bankHoldings = bankAgg.govBondHoldings,
      foreignHoldings = world.gov.foreignBondHoldings,
      nbpHoldings = world.nbp.govBondHoldings,
      insuranceHoldings = world.financial.insurance.govBondHoldings,
      ppkHoldings = world.social.ppk.bondHoldings,
      tfiHoldings = world.financial.nbfi.tfiGovBondHoldings,
    )
