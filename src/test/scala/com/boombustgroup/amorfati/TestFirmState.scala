package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.agents.{Firm, Region, TechState}
import com.boombustgroup.amorfati.types.*

object TestFirmState:

  case class Fixture(
      state: Firm.State,
      financialStocks: Firm.FinancialStocks,
  )

  def apply(
      id: FirmId = FirmId(0),
      cash: PLN = PLN.Zero,
      debt: PLN = PLN.Zero,
      tech: TechState = TechState.Traditional(10),
      riskProfile: Share = Share("0.5"),
      innovationCostFactor: Multiplier = Multiplier.One,
      digitalReadiness: Share = Share("0.3"),
      sector: SectorIdx = SectorIdx(0),
      neighbors: Vector[FirmId] = Vector.empty,
      bankId: BankId = BankId(0),
      equityRaised: PLN = PLN.Zero,
      initialSize: Int = 10,
      capitalStock: PLN = PLN.Zero,
      foreignOwned: Boolean = false,
      stateOwned: Boolean = false,
      inventory: PLN = PLN.Zero,
      greenCapital: PLN = PLN.Zero,
      accumulatedLoss: PLN = PLN.Zero,
      markup: Multiplier = Multiplier.One,
      region: Region = Region.Central,
      startupMonthsLeft: Int = 0,
      startupTargetWorkers: Int = 0,
      startupFilledWorkers: Int = 0,
      hiringSignalMonths: Int = 0,
  ): Firm.State =
    fixture(
      id = id,
      cash = cash,
      debt = debt,
      tech = tech,
      riskProfile = riskProfile,
      innovationCostFactor = innovationCostFactor,
      digitalReadiness = digitalReadiness,
      sector = sector,
      neighbors = neighbors,
      bankId = bankId,
      equityRaised = equityRaised,
      initialSize = initialSize,
      capitalStock = capitalStock,
      foreignOwned = foreignOwned,
      stateOwned = stateOwned,
      inventory = inventory,
      greenCapital = greenCapital,
      accumulatedLoss = accumulatedLoss,
      markup = markup,
      region = region,
      startupMonthsLeft = startupMonthsLeft,
      startupTargetWorkers = startupTargetWorkers,
      startupFilledWorkers = startupFilledWorkers,
      hiringSignalMonths = hiringSignalMonths,
    ).state

  def fixture(
      id: FirmId = FirmId(0),
      cash: PLN = PLN.Zero,
      debt: PLN = PLN.Zero,
      tech: TechState = TechState.Traditional(10),
      riskProfile: Share = Share("0.5"),
      innovationCostFactor: Multiplier = Multiplier.One,
      digitalReadiness: Share = Share("0.3"),
      sector: SectorIdx = SectorIdx(0),
      neighbors: Vector[FirmId] = Vector.empty,
      bankId: BankId = BankId(0),
      equityRaised: PLN = PLN.Zero,
      initialSize: Int = 10,
      capitalStock: PLN = PLN.Zero,
      foreignOwned: Boolean = false,
      stateOwned: Boolean = false,
      inventory: PLN = PLN.Zero,
      greenCapital: PLN = PLN.Zero,
      accumulatedLoss: PLN = PLN.Zero,
      markup: Multiplier = Multiplier.One,
      region: Region = Region.Central,
      startupMonthsLeft: Int = 0,
      startupTargetWorkers: Int = 0,
      startupFilledWorkers: Int = 0,
      hiringSignalMonths: Int = 0,
  ): Fixture =
    Fixture(
      state = Firm.State(
        id = id,
        tech = tech,
        riskProfile = riskProfile,
        innovationCostFactor = innovationCostFactor,
        digitalReadiness = digitalReadiness,
        sector = sector,
        neighbors = neighbors,
        bankId = bankId,
        initialSize = initialSize,
        capitalStock = capitalStock,
        foreignOwned = foreignOwned,
        stateOwned = stateOwned,
        inventory = inventory,
        greenCapital = greenCapital,
        accumulatedLoss = accumulatedLoss,
        markup = markup,
        region = region,
        startupMonthsLeft = startupMonthsLeft,
        startupTargetWorkers = startupTargetWorkers,
        startupFilledWorkers = startupFilledWorkers,
        hiringSignalMonths = hiringSignalMonths,
      ),
      financialStocks = financial(cash = cash, debt = debt, equityRaised = equityRaised),
    )

  def financial(
      cash: PLN = PLN.Zero,
      debt: PLN = PLN.Zero,
      equityRaised: PLN = PLN.Zero,
  ): Firm.FinancialStocks =
    Firm.FinancialStocks(
      cash = cash,
      firmLoan = debt,
      equity = equityRaised,
    )
