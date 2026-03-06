package sfc.engine

import sfc.agents.{Firm, FirmOps, TechState}
import sfc.config.Config
import sfc.types.*
import sfc.util.KahanSum.*

object IntermediateMarket:

  case class Result(firms: Array[Firm], totalPaid: Double)

  def process(firms: Array[Firm], sectorMults: Vector[Double], price: Double,
              ioMatrix: Vector[Vector[Double]],
              columnSums: Vector[Double],
              scale: Double = 1.0): Result =
    val nSectors = 6

    // Identify living firms and compute per-firm gross output
    val living = firms.indices.filter(i => FirmOps.isAlive(firms(i)))
    val grossOutput = new Array[Double](firms.length)
    for i <- living do
      grossOutput(i) = FirmOps.capacity(firms(i)) * sectorMults(firms(i).sector.toInt) * price

    // Total gross output per sector (for revenue distribution)
    val sectorOutput = new Array[Double](nSectors)
    for i <- living do
      sectorOutput(firms(i).sector.toInt) += grossOutput(i)

    // Firms can only buy from sectors that have living suppliers.
    // Effective column sum for sector j = Σ_{i: hasFirms(i)} a_ij
    val hasFirms = (0 until nSectors).map(i => sectorOutput(i) > 0)
    val effectiveColSums = (0 until nSectors).map { j =>
      (0 until nSectors).filter(i => hasFirms(i)).map(i => ioMatrix(i)(j)).sum
    }

    // Revenue for sector i = Σ_j a_ij × sectorOutput_j (only from sectors with firms)
    val cashAdj = new Array[Double](firms.length)
    var totalPaid = 0.0

    val sectorRevenue = new Array[Double](nSectors)
    for i <- 0 until nSectors if hasFirms(i) do
      for j <- 0 until nSectors do
        sectorRevenue(i) += ioMatrix(i)(j) * sectorOutput(j)

    // Distribute costs and revenues to individual firms
    for idx <- living do
      val f = firms(idx)
      val j = f.sector.toInt
      // Cost: intermediate purchases only from sectors with suppliers
      val ioCost = grossOutput(idx) * effectiveColSums(j)
      // Revenue: proportional to this firm's output within its sector
      val ioRevenue = if sectorOutput(f.sector.toInt) > 0 then
        sectorRevenue(f.sector.toInt) * (grossOutput(idx) / sectorOutput(f.sector.toInt))
      else 0.0
      cashAdj(idx) = (ioRevenue - ioCost) * scale
      totalPaid += ioCost * scale

    // Verify zero-sum (within floating-point tolerance)
    val totalAdj = cashAdj.kahanSum
    if Math.abs(totalAdj) > 1.0 then
      System.err.println(f"[IO] WARNING: non-zero-sum cash adjustment: $totalAdj%.2f")

    // Apply cash adjustments
    val newFirms = firms.clone()
    for idx <- living do
      val f = newFirms(idx)
      newFirms(idx) = f.copy(cash = f.cash + PLN(cashAdj(idx)))

    Result(newFirms, totalPaid)
