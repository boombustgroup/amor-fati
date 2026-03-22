package com.boombustgroup.amorfati.fp

/** Entity identifiers — Int-based, not fixed-point. */
object EntityIds:
  opaque type BankId = Int
  object BankId:
    inline def apply(i: Int): BankId            = i
    val NoBank: BankId                          = -1
    extension (b: BankId) inline def toInt: Int = b

  opaque type FirmId = Int
  object FirmId:
    inline def apply(i: Int): FirmId            = i
    extension (f: FirmId) inline def toInt: Int = f

  opaque type HhId = Int
  object HhId:
    inline def apply(i: Int): HhId            = i
    extension (h: HhId) inline def toInt: Int = h
    given Ordering[HhId]                      = Ordering.Int

  opaque type SectorIdx = Int
  object SectorIdx:
    inline def apply(i: Int): SectorIdx            = i
    extension (s: SectorIdx) inline def toInt: Int = s
