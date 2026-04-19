package com.boombustgroup.amorfati.engine.ledger

/** Stable runtime slots for non-agent fund buckets inside `EntitySector.Funds`.
  */
object FundRuntimeIndex:
  val Zus: Int           = 0
  val Nfz: Int           = 1
  val Ppk: Int           = 2
  val Fp: Int            = 3
  val Pfron: Int         = 4
  val Fgsp: Int          = 5
  val Jst: Int           = 6
  val CorpBondOther: Int = 7
  val Nbfi: Int          = 8
  val QuasiFiscal: Int   = 9

  val Count: Int = 10
