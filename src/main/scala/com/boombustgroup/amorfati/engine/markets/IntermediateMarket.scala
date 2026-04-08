package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Firm
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

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
      sectorMults: Vector[Multiplier],
      price: PriceIndex,
      ioMatrix: Vector[Vector[Share]],
      columnSums: Vector[Share],
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
    val arr      = in.firms.toArray

    // Identify living firms and compute per-firm gross output
    val living      = arr.indices.filter: i =>
      Firm.isAlive(arr(i))
    val grossOutput = Array.fill(arr.length)(PLN.Zero)
    for i <- living do grossOutput(i) = Firm.computeCapacity(arr(i)) * in.sectorMults(arr(i).sector.toInt) * in.price.toMultiplier

    // Total gross output per sector (for revenue distribution)
    val sectorOutput = Array.fill(nSectors)(PLN.Zero)
    for i <- living do sectorOutput(arr(i).sector.toInt) += grossOutput(i)

    // Firms can only buy from sectors that have living suppliers.
    // Effective column sum for sector j = Sum_{i: hasFirms(i)} a_ij
    val hasFirms         = (0 until nSectors).map: i =>
      sectorOutput(i) > PLN.Zero
    val effectiveColSums = (0 until nSectors).map: j =>
      (0 until nSectors).collect { case i if hasFirms(i) => in.ioMatrix(i)(j) }.foldLeft(Share.Zero)(_ + _)

    // Revenue for sector i = Sum_j a_ij * sectorOutput_j (only from sectors with firms)
    val cashAdj = Array.fill(arr.length)(PLN.Zero)

    val sectorRevenue = Array.fill(nSectors)(PLN.Zero)
    for i <- 0 until nSectors if hasFirms(i) do
      for j <- 0 until nSectors do sectorRevenue(i) += in.ioMatrix(i)(j) * sectorOutput(j)

    // Distribute costs and revenues to individual firms
    var totalPaidAcc = PLN.Zero
    for idx <- living do
      val f         = arr(idx)
      val j         = f.sector.toInt
      val ioCost    = (grossOutput(idx) * effectiveColSums(j)) * in.scale
      val ioRevenue =
        if sectorOutput(j) > PLN.Zero then sectorRevenue(j) * grossOutput(idx).ratioTo(sectorOutput(j)).toShare * in.scale
        else PLN.Zero
      cashAdj(idx) = ioRevenue - ioCost
      totalPaidAcc += ioCost

    // Verify zero-sum (within floating-point tolerance)
    val totalAdj = cashAdj.sum
    if totalAdj.abs > PLN.fromLong(1) then System.err.println("[IO] WARNING: non-zero-sum cash adjustment exceeded 1 PLN tolerance")

    // Apply cash adjustments
    val newFirms = arr.clone()
    for idx <- living do
      val f = newFirms(idx)
      newFirms(idx) = f.copy(cash = f.cash + cashAdj(idx))

    Result(newFirms.toVector, totalPaidAcc)
