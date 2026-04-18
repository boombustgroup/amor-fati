package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Firm
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.Distribute

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
  ):
    require(scale >= Multiplier.Zero, "IntermediateMarket.Input.scale must be non-negative")
    require(
      columnSums == ioMatrix.indices.map(j => ioMatrix.map(_(j)).foldLeft(Share.Zero)(_ + _)).toVector,
      "IntermediateMarket.Input.columnSums must match ioMatrix",
    )

  /** @param firms
    *   firms with cash adjusted for intermediate purchases/sales (zero-sum)
    * @param totalPaid
    *   aggregate intermediate input costs across all living firms
    * @param cashAdjustments
    *   per-firm cash deltas from intermediate purchases/sales
    */
  case class Result(firms: Vector[Firm.State], totalPaid: PLN, cashAdjustments: Vector[PLN])

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
    val hasFirms = (0 until nSectors).map: i =>
      sectorOutput(i) > PLN.Zero

    val cashAdj     = Array.fill(arr.length)(PLN.Zero)
    val sectorCosts = Array.fill(nSectors)(PLN.Zero)

    // Compute exact sector-to-sector flows once, then distribute by sector.
    // This keeps global costs and revenues aligned in raw PLN before firm-level allocation.
    val sectorRevenue = Array.fill(nSectors)(PLN.Zero)
    for
      i <- 0 until nSectors
      if hasFirms(i)
      j <- 0 until nSectors
    do
      val flow = (sectorOutput(j) * in.ioMatrix(i)(j)) * in.scale
      sectorRevenue(i) += flow
      sectorCosts(j) += flow

    val sectorMembers = (0 until nSectors).map: sector =>
      living.filter(idx => arr(idx).sector.toInt == sector).toArray

    for j <- 0 until nSectors do
      val members = sectorMembers(j)
      if members.nonEmpty && sectorCosts(j) > PLN.Zero then
        val weights     = members.map(idx => grossOutput(idx).distributeRaw)
        val allocations = Distribute.distribute(sectorCosts(j).distributeRaw, weights)
        members.indices.foreach: pos =>
          cashAdj(members(pos)) -= PLN.fromRaw(allocations(pos))

    for i <- 0 until nSectors do
      val members = sectorMembers(i)
      if members.nonEmpty && sectorRevenue(i) > PLN.Zero then
        val weights     = members.map(idx => grossOutput(idx).distributeRaw)
        val allocations = Distribute.distribute(sectorRevenue(i).distributeRaw, weights)
        members.indices.foreach: pos =>
          cashAdj(members(pos)) += PLN.fromRaw(allocations(pos))

    // Verify zero-sum (within floating-point tolerance)
    val totalAdj = cashAdj.sum
    if totalAdj.abs > PLN.fromLong(1) then System.err.println("[IO] WARNING: non-zero-sum cash adjustment exceeded 1 PLN tolerance")

    // Apply cash adjustments
    val newFirms = arr.clone()
    for idx <- living do
      val f = newFirms(idx)
      newFirms(idx) = f.copy(cash = f.cash + cashAdj(idx))

    Result(newFirms.toVector, sectorCosts.foldLeft(PLN.Zero)(_ + _), cashAdj.toVector)
