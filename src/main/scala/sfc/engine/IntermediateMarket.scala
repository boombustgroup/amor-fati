package sfc.engine

import sfc.agents.{Firm, TechState}
import sfc.types.*
import sfc.util.KahanSum.*

object IntermediateMarket:

  case class Result(firms: Vector[Firm.State], totalPaid: Double)

  def process(
    firms: Vector[Firm.State],
    sectorMults: Vector[Double],
    price: Double,
    ioMatrix: Vector[Vector[Double]],
    columnSums: Vector[Double],
    scale: Double = 1.0,
  ): Result =
    val nSectors = 6
    val arr = firms.toArray

    // Identify living firms and compute per-firm gross output
    val living = arr.indices.filter(i => Firm.isAlive(arr(i)))
    val grossOutput = new Array[Double](arr.length)
    for i <- living do grossOutput(i) = Firm.capacity(arr(i)) * sectorMults(arr(i).sector.toInt) * price

    // Total gross output per sector (for revenue distribution)
    val sectorOutput = new Array[Double](nSectors)
    for i <- living do sectorOutput(arr(i).sector.toInt) += grossOutput(i)

    // Firms can only buy from sectors that have living suppliers.
    // Effective column sum for sector j = Σ_{i: hasFirms(i)} a_ij
    val hasFirms = (0 until nSectors).map(i => sectorOutput(i) > 0)
    val effectiveColSums = (0 until nSectors).map { j =>
      (0 until nSectors).filter(i => hasFirms(i)).map(i => ioMatrix(i)(j)).sum
    }

    // Revenue for sector i = Σ_j a_ij × sectorOutput_j (only from sectors with firms)
    val cashAdj = new Array[Double](arr.length)
    var totalPaid = 0.0

    val sectorRevenue = new Array[Double](nSectors)
    for i <- 0 until nSectors if hasFirms(i) do
      for j <- 0 until nSectors do sectorRevenue(i) += ioMatrix(i)(j) * sectorOutput(j)

    // Distribute costs and revenues to individual firms
    for idx <- living do
      val f = arr(idx)
      val j = f.sector.toInt
      // Cost: intermediate purchases only from sectors with suppliers
      val ioCost = grossOutput(idx) * effectiveColSums(j)
      // Revenue: proportional to this firm's output within its sector
      val ioRevenue =
        if sectorOutput(f.sector.toInt) > 0 then
          sectorRevenue(f.sector.toInt) * (grossOutput(idx) / sectorOutput(f.sector.toInt))
        else 0.0
      cashAdj(idx) = (ioRevenue - ioCost) * scale
      totalPaid += ioCost * scale

    // Verify zero-sum (within floating-point tolerance)
    val totalAdj = cashAdj.kahanSum
    if Math.abs(totalAdj) > 1.0 then System.err.println(f"[IO] WARNING: non-zero-sum cash adjustment: $totalAdj%.2f")

    // Apply cash adjustments
    val newFirms = arr.clone()
    for idx <- living do
      val f = newFirms(idx)
      newFirms(idx) = f.copy(cash = f.cash + PLN(cashAdj(idx)))

    Result(newFirms.toVector, totalPaid)
