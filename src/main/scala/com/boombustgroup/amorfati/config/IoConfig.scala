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
    matrix: Vector[Vector[Double]] = IoConfig.DefaultMatrix,
    scale: Ratio = Ratio(1.0),
):
  /** Pre-computed column sums of the technical coefficients matrix (used in
    * intermediate demand calculation).
    */
  val columnSums: Vector[Double] =
    (0 until 6).map(j => matrix.map(_(j)).sum).toVector

object IoConfig:
  /** Default 6x6 I-O technical coefficients matrix (GUS supply-use tables
    * 2024).
    *
    * Rows/columns: BPO/SSC, Manufacturing, Retail/Services, Healthcare, Public,
    * Agriculture.
    */
  val DefaultMatrix: Vector[Vector[Double]] = Vector(
    Vector(0.05, 0.03, 0.04, 0.02, 0.03, 0.01),
    Vector(0.04, 0.35, 0.12, 0.15, 0.05, 0.18),
    Vector(0.15, 0.10, 0.12, 0.08, 0.07, 0.08),
    Vector(0.01, 0.00, 0.01, 0.05, 0.02, 0.01),
    Vector(0.01, 0.01, 0.01, 0.01, 0.03, 0.01),
    Vector(0.00, 0.08, 0.05, 0.01, 0.01, 0.12),
  )
