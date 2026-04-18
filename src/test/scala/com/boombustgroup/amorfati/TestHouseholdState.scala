package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.agents.{ContractType, HhStatus, Household, Region}
import com.boombustgroup.amorfati.types.*

object TestHouseholdState:

  def apply(
      id: HhId = HhId(0),
      savings: PLN = PLN.Zero,
      debt: PLN = PLN.Zero,
      monthlyRent: PLN = PLN.Zero,
      skill: Share = Share(0.8),
      healthPenalty: Share = Share.Zero,
      mpc: Share = Share(0.8),
      status: HhStatus = HhStatus.Unemployed(0),
      socialNeighbors: Array[HhId] = Array.empty,
      bankId: BankId = BankId(0),
      equityWealth: PLN = PLN.Zero,
      lastSectorIdx: SectorIdx = SectorIdx(-1),
      isImmigrant: Boolean = false,
      numDependentChildren: Int = 0,
      consumerDebt: PLN = PLN.Zero,
      education: Int = 2,
      taskRoutineness: Share = Share(0.5),
      wageScar: Share = Share.Zero,
      financialDistressMonths: Int = 0,
      contractType: ContractType = ContractType.Permanent,
      region: Region = Region.Central,
  ): Household.State =
    Household.State(
      id = id,
      financial = Household.FinancialStocks(
        demandDeposit = savings,
        mortgageLoan = debt,
        consumerLoan = consumerDebt,
        equity = equityWealth,
      ),
      monthlyRent = monthlyRent,
      skill = skill,
      healthPenalty = healthPenalty,
      mpc = mpc,
      status = status,
      socialNeighbors = socialNeighbors,
      bankId = bankId,
      lastSectorIdx = lastSectorIdx,
      isImmigrant = isImmigrant,
      numDependentChildren = numDependentChildren,
      education = education,
      taskRoutineness = taskRoutineness,
      wageScar = wageScar,
      financialDistressMonths = financialDistressMonths,
      contractType = contractType,
      region = region,
    )
