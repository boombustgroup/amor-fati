package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Capital flight and hot money: risk-off, carry trade, EM sentiment.
  *
  * Extends the mechanical portfolio flow model with three crisis channels:
  *
  *   1. '''Global risk-off''' — exogenous EM sentiment shock (VIX spike, Fed
  *      tightening, geopolitical event). Triggers sudden capital outflow. PLN
  *      can depreciate 10-15% in weeks (2022, 2020). Configurable shock month
  *      and magnitude.
  *   2. '''Carry trade''' — leveraged positions accumulate when yield spread
  *      exceeds threshold. On risk-off: forced unwind → amplified PLN selling →
  *      overshoot beyond fundamentals. Stock builds gradually, unwinds
  *      suddenly.
  *   3. '''Bond market signal''' — low bid-to-cover in SPW auction erodes
  *      foreign confidence → additional portfolio outflow. Links BondAuction to
  *      capital account.
  *
  * Pure function — no mutable state beyond carry trade stock (tracked in
  * BopState). Called from OpenEconomy.step.
  *
  * Calibration: NBP BoP data, BIS carry trade estimates, IMF capital flow at
  * risk methodology.
  */
object CapitalFlows:

  /** Carry trade state: accumulated leveraged positions. */
  case class CarryState(
      stock: PLN, // outstanding carry trade positions (builds when spread high)
  )
  object CarryState:
    val zero: CarryState = CarryState(PLN.Zero)

  /** Result of capital flow computation. */
  case class Result(
      riskOffOutflow: PLN, // outflow from global risk-off event
      carryTradeFlow: PLN, // net carry trade flow (positive = inflow, negative = unwind)
      auctionSignal: PLN,  // outflow from weak bond auction
      newCarryState: CarryState,
  ):
    def totalAdjustment: PLN = riskOffOutflow + carryTradeFlow + auctionSignal

  /** Compute capital flight adjustments for this month.
    *
    * @param month
    *   current simulation month
    * @param yieldSpread
    *   domestic yield − foreign yield
    * @param bidToCover
    *   SPW auction bid-to-cover ratio
    * @param prevCarry
    *   previous carry trade state
    * @param monthlyGdp
    *   GDP proxy for scaling
    */
  def compute(
      month: Int,
      yieldSpread: Rate,
      bidToCover: Multiplier,
      prevCarry: CarryState,
      monthlyGdp: PLN,
  )(using p: SimParams): Result =
    // 1. Global risk-off shock
    val riskOff =
      if p.forex.riskOffShockMonth > 0 && month == p.forex.riskOffShockMonth then PLN(monthlyGdp.toDouble * p.forex.riskOffMagnitude.toDouble * -1.0)
      else PLN.Zero

    // 2. Carry trade: accumulate when spread > threshold, unwind on risk-off
    val spreadAboveThreshold = (yieldSpread - p.forex.carryThreshold).max(Rate.Zero)
    val isRiskOff            = p.forex.riskOffShockMonth > 0 && month >= p.forex.riskOffShockMonth &&
      month < p.forex.riskOffShockMonth + p.forex.riskOffDurationMonths
    val carryAccumulation    =
      if !isRiskOff then PLN(monthlyGdp.toDouble * spreadAboveThreshold.toDouble * p.forex.carryAccumulationRate.toDouble)
      else PLN.Zero
    val carryUnwind          =
      if isRiskOff then PLN(prevCarry.stock.toDouble * p.forex.carryUnwindSpeed.toDouble * -1.0)
      else PLN.Zero
    val newStock             = (prevCarry.stock + carryAccumulation + carryUnwind).max(PLN.Zero)
    val carryFlow            = carryAccumulation + carryUnwind

    // 3. Bond auction signal: low bid-to-cover → confidence erosion
    val auctionOutflow =
      if bidToCover.toDouble < p.forex.auctionConfidenceThreshold.toDouble then
        PLN(monthlyGdp.toDouble * p.forex.auctionOutflowSensitivity.toDouble * (p.forex.auctionConfidenceThreshold.toDouble - bidToCover.toDouble) * -1.0)
      else PLN.Zero

    Result(riskOff, carryFlow, auctionOutflow, CarryState(newStock))
