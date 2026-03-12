package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.engine.Simulation
import com.boombustgroup.amorfati.montecarlo.SimOutput.Col
import com.boombustgroup.amorfati.util.KahanSum.*

/** Zero-cost typed wrappers for Monte Carlo simulation output. */

/** Result of a single simulation run. */
case class RunResult(
    timeSeries: TimeSeries,
    terminalState: Simulation.SimState,
)

// ---------------------------------------------------------------------------
//  TimeSeries — opaque over Array[Array[Double]]
// ---------------------------------------------------------------------------

opaque type TimeSeries = Array[Array[Double]]

object TimeSeries:
  inline def wrap(raw: Array[Array[Double]]): TimeSeries = raw

  extension (ts: TimeSeries)
    /** Type-safe access: `ts.at(month, Col.Inflation)`. */
    inline def at(month: Int, col: Col): Double = ts(month)(col.ordinal)

    /** Raw row for a given month. */
    inline def monthRow(month: Int): Array[Double] = ts(month)

    /** Last row of the series. */
    inline def lastMonth: Array[Double] = ts(ts.length - 1)

    /** Number of months in the series. */
    inline def nMonths: Int = ts.length

    // ---- Source-compat with Array[Array[Double]] (tests, existing call sites) ----
    inline def length: Int                             = ts.length
    inline def indices: Range                          = 0 until ts.length
    inline def apply(month: Int): Array[Double]        = ts(month)
    inline def foreach(f: Array[Double] => Unit): Unit =
      var i = 0
      while i < ts.length do { f(ts(i)); i += 1 }
    def map[B](f: Array[Double] => B): Vector[B]       =
      val b = Vector.newBuilder[B]
      b.sizeHint(ts.length)
      var i = 0
      while i < ts.length do { b += f(ts(i)); i += 1 }
      b.result()

// ---------------------------------------------------------------------------
//  DescriptiveStats — single source of truth for mean/std/p05/p95
// ---------------------------------------------------------------------------

/** Summary statistics for a 1-D sample. */
case class DescriptiveStats(mean: Double, std: Double, p05: Double, p95: Double)

object DescriptiveStats:
  /** Compute from an unsorted array. Sorts in-place, then computes stats via
    * Kahan summation.
    */
  def from(values: Array[Double]): DescriptiveStats =
    java.util.Arrays.sort(values)
    fromSorted(values)

  /** Compute from a **pre-sorted** array. Package-private — callers outside
    * `sfc` must use [[from]].
    */
  private[amorfati] def fromSorted(sorted: Array[Double]): DescriptiveStats =
    val n        = sorted.length
    val mean     = sorted.kahanSum / n
    val variance = sorted.kahanSumBy(v => (v - mean) * (v - mean)) / n
    val std      = Math.sqrt(variance)
    val p05      = sorted((n * 0.05).toInt)
    val p95      = sorted(Math.min(n - 1, (n * 0.95).toInt))
    DescriptiveStats(mean, std, p05, p95)

// ---------------------------------------------------------------------------
//  McResults — immutable Monte Carlo output
// ---------------------------------------------------------------------------

/** Immutable container for all Monte Carlo run results. */
case class McResults(runs: Vector[RunResult]):
  def nSeeds: Int = runs.length

  /** Sorted terminal values for a given column. */
  def terminalValues(col: Col): Array[Double] =
    val arr = runs.map(r => r.timeSeries.at(r.timeSeries.nMonths - 1, col)).toArray
    java.util.Arrays.sort(arr)
    arr

  /** Cross-seed statistics for all columns at a given month (batch — single
    * pass).
    */
  def crossSeedStatsAll(month: Int, nCols: Int): Array[DescriptiveStats] =
    val n      = runs.length
    val result = new Array[DescriptiveStats](nCols)
    val buf    = new Array[Double](n)
    for c <- 0 until nCols do
      var i = 0
      while i < n do
        buf(i) = runs(i).timeSeries(month)(c)
        i += 1
      java.util.Arrays.sort(buf)
      result(c) = DescriptiveStats.fromSorted(buf)
    result
