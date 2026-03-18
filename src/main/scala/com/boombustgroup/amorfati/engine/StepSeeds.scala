package com.boombustgroup.amorfati.engine

/** Per-component RNG isolation via deterministic seed derivation.
  *
  * Each simulation component gets a deterministic sub-seed derived from
  * `(masterSeed, month, componentId)`. This ensures that adding a
  * `Random.nextDouble()` call in one component does not shift the RNG sequence
  * of any other component — full isolation with zero shared mutable state.
  *
  * Component IDs are stable across engine versions: new components get new IDs,
  * existing IDs never change.
  */
object StepSeeds:

  // Component IDs — stable across versions, never reuse
  val Household: Int     = 0
  val Firm: Int          = 1
  val Rewire: Int        = 2
  val WorldAssembly: Int = 3
  val LaborSearch: Int   = 4
  val Immigration: Int   = 5
  val BankLend: Int      = 6
  val Commodity: Int     = 7
  val DepositFlight: Int = 8

  /** Derive a deterministic sub-seed for a component in a given month.
    *
    * Uses Murmur-style mixing: cheap, good avalanche, fully deterministic. Same
    * `(masterSeed, month, componentId)` always produces the same output.
    */
  inline def derive(masterSeed: Long, month: Int, componentId: Int): Long =
    var h = masterSeed ^ (month.toLong * 0x9e3779b97f4a7c15L)
    h ^= componentId.toLong * 0x517cc1b727220a95L
    h ^= (h >>> 33)
    h *= 0xff51afd7ed558ccdL
    h ^= (h >>> 33)
    h *= 0xc4ceb9fe1a85ec53L
    h ^= (h >>> 33)
    h
