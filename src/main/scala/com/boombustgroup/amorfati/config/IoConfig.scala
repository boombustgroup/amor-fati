package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Input-Output matrix for inter-sectoral intermediate demand.
  *
  * Implements the 6x6 technical coefficients matrix A (Leontief, 1936) where
  * `matrix(i)(j)` is sector i's share of intermediate purchases from sector j.
  * Inter-sector purchases are deposit transfers within the same bank (zero-sum
  * for total deposits), so they do not break existing SFC identities. Column
  * sums are pre-computed for efficiency.
  *
  * Default matrix calibrated to GUS supply-use tables 2024.
  *
  * @param matrix
  *   6x6 technical coefficients matrix A[i][j] = sector i's input share from
  *   sector j
  * @param scale
  *   scaling factor for I-O flows (1.0 = full strength, for sensitivity
  *   analysis)
  */
case class IoConfig(
    matrix: Vector[Vector[Share]] = IoConfig.DefaultMatrix,
    scale: Multiplier = Multiplier(1.0),
):
  /** Pre-computed column sums of the technical coefficients matrix (used in
    * intermediate demand calculation).
    */
  val columnSums: Vector[Share] =
    (0 until 6).map(j => matrix.map(_(j)).foldLeft(Share.Zero)(_ + _)).toVector

object IoConfig:
  /** Default 6x6 I-O technical coefficients matrix (GUS supply-use tables
    * 2024).
    *
    * Rows/columns: BPO/SSC, Manufacturing, Retail/Services, Healthcare, Public,
    * Agriculture.
    */
  val DefaultMatrix: Vector[Vector[Share]] = Vector(
    Vector(Share(0.05), Share(0.03), Share(0.04), Share(0.02), Share(0.03), Share(0.01)),
    Vector(Share(0.04), Share(0.35), Share(0.12), Share(0.15), Share(0.05), Share(0.18)),
    Vector(Share(0.15), Share(0.10), Share(0.12), Share(0.08), Share(0.07), Share(0.08)),
    Vector(Share(0.01), Share.Zero, Share(0.01), Share(0.05), Share(0.02), Share(0.01)),
    Vector(Share(0.01), Share(0.01), Share(0.01), Share(0.01), Share(0.03), Share(0.01)),
    Vector(Share.Zero, Share(0.08), Share(0.05), Share(0.01), Share(0.01), Share(0.12)),
  )
