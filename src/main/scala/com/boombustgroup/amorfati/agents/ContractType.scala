package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*

/** Employment contract types in the Polish dual labor market.
  *
  * ~30% of Polish employment is non-standard (GUS LFS 2024). Contract type
  * determines: (1) social contribution rates (ZUS/FP), (2) firing costs and
  * order, (3) vulnerability to AI displacement.
  *
  * Dual labor market dynamics: flexible segment (Zlecenie/B2B) absorbs shocks
  * via instant hire/fire. Rigid segment (Permanent) resists adjustment but
  * provides income stability. AI hits flexible workers first — routine tasks on
  * non-standard contracts are easiest to automate and cheapest to terminate.
  *
  * Calibration: GUS LFS 2024, ZUS contribution tables, Kodeks Pracy.
  */
enum ContractType:
  /** Umowa o pracę — full employment protection (Kodeks Pracy). Full ZUS
    * (19.52% employer), FP (2.45%), severance on firing.
    */
  case Permanent

  /** Umowa zlecenie — civil law contract, partial protection. Partial ZUS
    * (~13%), no FP, no severance, easy to terminate.
    */
  case Zlecenie

  /** Samozatrudnienie / B2B — self-employment contract. Flat ZUS (ryczałt ~1400
    * PLN/mo), no employer ZUS/FP, instant termination. Highest gross-to-net
    * ratio, lowest protection.
    */
  case B2B

object ContractType:

  /** Per-contract ZUS employer contribution rate. Permanent: full 19.52%,
    * Zlecenie: ~13%, B2B: 0% (self-employed pays own).
    */
  def zusEmployerRate(ct: ContractType): Share = ct match
    case Permanent => Share.decimal(1952, 4)
    case Zlecenie  => Share.decimal(13, 2)
    case B2B       => Share.Zero

  /** Per-contract FP contribution rate. Permanent: 2.45%, Zlecenie/B2B: 0%.
    */
  def fpRate(ct: ContractType): Share = ct match
    case Permanent => Share.decimal(245, 4)
    case _         => Share.Zero

  /** Firing priority: lower = fired first during downsizing. B2B (0) → Zlecenie
    * (1) → Permanent (2).
    */
  def firingPriority(ct: ContractType): Int = ct match
    case B2B       => 0
    case Zlecenie  => 1
    case Permanent => 2

  /** AI displacement vulnerability: higher = more vulnerable. B2B routine tasks
    * easiest to automate + cheapest to terminate.
    */
  def aiVulnerability(ct: ContractType): Multiplier = ct match
    case B2B       => Multiplier.decimal(15, 1)
    case Zlecenie  => Multiplier.decimal(12, 1)
    case Permanent => Multiplier.One

  /** Sector-specific probability of each contract type for new hires. Returns
    * (pPermanent, pZlecenie, pB2B) summing to 1. GUS LFS 2024 sector breakdown.
    */
  def sectorMix(sectorIdx: Int): (Share, Share, Share) = sectorIdx match
    case 0 => (Share.decimal(30, 2), Share.decimal(20, 2), Share.decimal(50, 2)) // BPO/SSC — high B2B
    case 1 => (Share.decimal(75, 2), Share.decimal(15, 2), Share.decimal(10, 2)) // Manufacturing — mostly permanent
    case 2 => (Share.decimal(50, 2), Share.decimal(35, 2), Share.decimal(15, 2)) // Retail/Services — mixed
    case 3 => (Share.decimal(80, 2), Share.decimal(15, 2), Share.decimal(5, 2))  // Healthcare — mostly permanent
    case 4 => (Share.decimal(90, 2), Share.decimal(5, 2), Share.decimal(5, 2))   // Public — almost all permanent
    case 5 => (Share.decimal(40, 2), Share.decimal(40, 2), Share.decimal(20, 2)) // Agriculture — high zlecenie
    case _ => (Share.decimal(70, 2), Share.decimal(20, 2), Share.decimal(10, 2)) // default

  /** Draw a sector-specific employment contract. */
  def sampleForSector(sectorIdx: SectorIdx, rng: RandomStream): ContractType =
    val (permanent, zlecenie, _) = sectorMix(sectorIdx.toInt)
    val draw                     = Share.random(rng)
    if draw < permanent then Permanent
    else if draw < permanent + zlecenie then Zlecenie
    else B2B
