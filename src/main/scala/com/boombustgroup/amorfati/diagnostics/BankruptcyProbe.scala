package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.{MonthDriver, MonthRandomness}
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}

object BankruptcyProbe:

  private def bankruptReason(f: Firm.State): Option[BankruptReason] = f.tech match
    case TechState.Bankrupt(reason) => Some(reason)
    case _                          => None

  @main def runBankruptcyProbe(seed: Long = 1L, months: Int = 12): Unit =
    given SimParams = SimParams.defaults

    val initState = FlowSimulation.SimState.fromInit(WorldInit.initialize(InitRandomness.Contract.fromSeed(seed)))

    println(s"seed=$seed months=$months")

    MonthDriver
      .unfoldSteps(initState): state =>
        Some(MonthRandomness.Contract.fromSeed(seed * 1000L + state.world.month.toLong + 1L))
      .take(months)
      .foreach: result =>
        val month        = result.nextState.world.month
        val prevById     = result.stateIn.firms.map(f => f.id -> f).toMap
        val newBankrupts = result.nextState.firms.flatMap: f =>
          bankruptReason(f).flatMap: reason =>
            prevById.get(f.id).flatMap(bankruptReason) match
              case Some(_) => None
              case None    => Some((reason, f.sector.toInt))

        val byReason    = newBankrupts.groupMapReduce(_._1)(_ => 1)(_ + _).toVector.sortBy(-_._2)
        val bySector    = newBankrupts.groupMapReduce(_._2)(_ => 1)(_ + _).toVector.sortBy(_._1)
        val unemp       = result.nextState.householdAggregates.unemploymentRate(result.nextState.world.derivedTotalPopulation)
        val demandMults = result.nextState.world.pipeline.sectorDemandMult

        println(
          s"month=$month unemp=$unemp deaths=${newBankrupts.size} demand2=${demandMults(2)} demand3=${demandMults(3)} demand4=${demandMults(4)}",
        )
        if newBankrupts.nonEmpty then
          println(s"  reasons=${byReason.mkString(", ")}")
          println(s"  sectors=${bySector.mkString(", ")}")
