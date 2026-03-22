package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Firm
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

/** Inter-sectoral intermediate demand via Leontief Input-Output (Leontief
  * 1936).
  *
  * Each living firm purchases intermediate inputs from other sectors according
  * to the technical coefficients matrix A (calibrated to GUS supply-use tables
  * 2024). Revenue from intermediate sales is distributed proportionally to firm
  * gross output within each sector.
  *
  * Cash adjustments are zero-sum across all living firms: total intermediate
  * costs equal total intermediate revenues (deposit transfers, no money
  * creation).
  *
  * Performance note: inner loops use mutable Arrays for O(N × K) throughput
  * where N = firm count, K = sector count. Immutable boundaries enforced on
  * Input/Result.
  */
object IntermediateMarket:

  case class Input(
      firms: Vector[Firm.State],
      sectorMults: Vector[Double],
      price: Double,
      ioMatrix: Vector[Vector[Double]],
      columnSums: Vector[Double],
      scale: Multiplier = Multiplier.One,
  )

  /** @param firms
    *   firms with cash adjusted for intermediate purchases/sales (zero-sum)
    * @param totalPaid
    *   aggregate intermediate input costs across all living firms
    */
  case class Result(firms: Vector[Firm.State], totalPaid: PLN)

  def process(in: Input)(using SimParams): Result =
    val nSectors = in.ioMatrix.size
    val scale    = in.scale.toDouble
    val arr      = in.firms.toArray

    // Identify living firms and compute per-firm gross output
    val living      = arr.indices.filter: i =>
      Firm.isAlive(arr(i))
    val grossOutput = new Array[Double](arr.length)
    for i <- living do grossOutput(i) = Firm.computeCapacity(arr(i)).toDouble * in.sectorMults(arr(i).sector.toInt) * in.price

    // Total gross output per sector (for revenue distribution)
    val sectorOutput = new Array[Double](nSectors)
    for i <- living do sectorOutput(arr(i).sector.toInt) += grossOutput(i)

    // Firms can only buy from sectors that have living suppliers.
    // Effective column sum for sector j = Sum_{i: hasFirms(i)} a_ij
    val hasFirms         = (0 until nSectors).map: i =>
      sectorOutput(i) > 0
    val effectiveColSums = (0 until nSectors).map: j =>
      (0 until nSectors).collect { case i if hasFirms(i) => in.ioMatrix(i)(j) }.sum

    // Revenue for sector i = Sum_j a_ij * sectorOutput_j (only from sectors with firms)
    val cashAdj = new Array[Double](arr.length)

    val sectorRevenue = new Array[Double](nSectors)
    for i <- 0 until nSectors if hasFirms(i) do for j <- 0 until nSectors do sectorRevenue(i) += in.ioMatrix(i)(j) * sectorOutput(j)

    // Distribute costs and revenues to individual firms
    var totalPaidAcc = 0.0
    for idx <- living do
      val f         = arr(idx)
      val j         = f.sector.toInt
      val ioCost    = grossOutput(idx) * effectiveColSums(j)
      val ioRevenue =
        if sectorOutput(j) > 0 then sectorRevenue(j) * (grossOutput(idx) / sectorOutput(j))
        else 0.0
      cashAdj(idx) = (ioRevenue - ioCost) * scale
      totalPaidAcc += ioCost * scale

    // Verify zero-sum (within floating-point tolerance)
    val totalAdj = cashAdj.kahanSum
    if Math.abs(totalAdj) > 1.0 then System.err.println(f"[IO] WARNING: non-zero-sum cash adjustment: $totalAdj%.2f")

    // Apply cash adjustments
    val newFirms = arr.clone()
    for idx <- living do
      val f = newFirms(idx)
      newFirms(idx) = f.copy(cash = f.cash + PLN(cashAdj(idx)))

    Result(newFirms.toVector, PLN(totalPaidAcc))
