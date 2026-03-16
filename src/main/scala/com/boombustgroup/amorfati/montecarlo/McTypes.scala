package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.engine.Simulation
import com.boombustgroup.amorfati.montecarlo.SimOutput.Col

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
