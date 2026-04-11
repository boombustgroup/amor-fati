package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.accounting.{InitCheck, Sfc}
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.montecarlo.SimOutput.Col

/** Zero-cost typed wrappers for Monte Carlo simulation output. */

/** Typed simulation errors — no exceptions, propagate via Either/ZIO. */
sealed trait SimError
object SimError:
  case class Init(errors: Vector[InitCheck.InitCheckResult]) extends SimError:
    override def toString: String =
      s"Init validation failed:\n${errors.map(e => s"  ${e.identity}: expected=${e.expected}, actual=${e.actual}").mkString("\n")}"

  case class SfcViolation(month: ExecutionMonth, errors: Vector[Sfc.SfcIdentityError]) extends SimError:
    override def toString: String =
      s"SFC violation at M${month.toInt}:\n${errors.map(e => s"  ${e.identity}: ${e.msg} (expected=${e.expected}, actual=${e.actual}, diff=${(e.actual - e.expected).abs})").mkString("\n")}"

/** Result of a single simulation run. */
case class RunResult(
    timeSeries: TimeSeries,
    terminalState: FlowSimulation.SimState,
)

// ---------------------------------------------------------------------------
//  TimeSeries — opaque over Array[Array[Double]]
// ---------------------------------------------------------------------------

opaque type TimeSeries = Array[Array[Double]]

object TimeSeries:
  inline def wrap(raw: Array[Array[Double]]): TimeSeries = raw

  private def rowIndex(ts: TimeSeries, month: ExecutionMonth): Int =
    val idx = month.previousCompleted.toInt
    require(idx < ts.length, s"ExecutionMonth M${month.toInt} outside TimeSeries of ${ts.length} months")
    idx

  extension (ts: TimeSeries)
    /** Type-safe access: `ts.at(month, Col.Inflation)`. */
    inline def at(month: ExecutionMonth, col: Col): Double = ts(rowIndex(ts, month))(col.ordinal)

    /** Raw row for a realized execution month. */
    inline def monthRow(month: ExecutionMonth): Array[Double] = ts(rowIndex(ts, month))

    /** Execution months materialized in this series, starting from M1. */
    def executionMonths: Vector[ExecutionMonth] =
      Vector.tabulate(ts.length)(offset => ExecutionMonth.First.advanceBy(offset))

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
