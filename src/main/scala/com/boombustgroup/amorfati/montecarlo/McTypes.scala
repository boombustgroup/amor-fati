package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.accounting.{InitCheck, Sfc}
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.montecarlo.McTimeseriesSchema.Col

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

  case class RuntimeFailure(operation: String, details: String) extends SimError:
    override def toString: String =
      s"Runtime failure during $operation: $details"

  case class OutputFailure(operation: String, path: String, details: String) extends SimError:
    override def toString: String =
      s"Output failure during $operation at $path: $details"

/** Result of a single simulation run. */
case class RunResult(
    timeSeries: TimeSeries,
    terminalState: FlowSimulation.SimState,
)

// ---------------------------------------------------------------------------
//  TimeSeries — opaque over Array[Array[MetricValue]]
// ---------------------------------------------------------------------------

opaque type TimeSeries = Array[Array[MetricValue]]

object TimeSeries:
  /** Wrap raw Monte Carlo output rows without runtime overhead. */
  inline def wrap(raw: Array[Array[MetricValue]]): TimeSeries = raw

  /** Convert a realized execution month to the underlying zero-based row index.
    */
  private def rowIndex(ts: TimeSeries, month: ExecutionMonth): Int =
    val idx = month.previousCompleted.toInt
    require(idx < ts.length, s"ExecutionMonth M${month.toInt} outside TimeSeries of ${ts.length} months")
    idx

  extension (ts: TimeSeries)
    /** Read one typed cell from a realized execution month. */
    inline def at(month: ExecutionMonth, col: Col): MetricValue = ts(rowIndex(ts, month))(col.ordinal)

    /** Raw Monte Carlo row for a realized execution month. */
    inline def monthRow(month: ExecutionMonth): Array[MetricValue] = ts(rowIndex(ts, month))

    /** All realized execution months materialized in this series, starting from
      * M1.
      */
    def executionMonths: Vector[ExecutionMonth] =
      Vector.tabulate(ts.length)(offset => ExecutionMonth.First.advanceBy(offset))

    /** Raw row for the latest realized month in the run. */
    inline def lastMonth: Array[MetricValue] = ts(ts.length - 1)

    /** Number of realized months stored in the series. */
    inline def nMonths: Int = ts.length

    /** Zero-based row indices for compatibility with collection-style
      * iteration.
      */
    inline def indices: Range = 0 until ts.length

    /** Iterate through raw rows without exposing the backing array type. */
    inline def foreach(f: Array[MetricValue] => Unit): Unit =
      var i = 0
      while i < ts.length do { f(ts(i)); i += 1 }

    /** Map over raw rows while preserving the opaque TimeSeries wrapper. */
    def map[B](f: Array[MetricValue] => B): Vector[B] =
      val b = Vector.newBuilder[B]
      b.sizeHint(ts.length)
      var i = 0
      while i < ts.length do { b += f(ts(i)); i += 1 }
      b.result()
