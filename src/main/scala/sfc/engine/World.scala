package sfc.engine

import sfc.agents.{HhState, HhAggregates, Household, NbpState}
import sfc.sfc.{GovState, BankState, ForexState}

case class World(
  month: Int,
  inflation: Double,
  priceLevel: Double,
  demandMultiplier: Double,
  gov: GovState,
  nbp: NbpState,
  bank: BankState,
  forex: ForexState,
  hh: HhState,
  automationRatio: Double,
  hybridRatio: Double,
  gdpProxy: Double,
  currentSigmas: Vector[Double],
  hhAgg: Option[HhAggregates] = None,
  households: Option[Vector[Household]] = None
)
