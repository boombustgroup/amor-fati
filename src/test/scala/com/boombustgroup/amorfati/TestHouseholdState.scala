package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.agents.{ContractType, HhStatus, Household, Region}
import com.boombustgroup.amorfati.types.*

object TestHouseholdState:

  case class Fixture(
      state: Household.State,
      financialStocks: Household.FinancialStocks,
  )

  def financial(
      savings: PLN = PLN.Zero,
      debt: PLN = PLN.Zero,
      consumerDebt: PLN = PLN.Zero,
      equityWealth: PLN = PLN.Zero,
  ): Household.FinancialStocks =
    Household.FinancialStocks(
      demandDeposit = savings,
      mortgageLoan = debt,
      consumerLoan = consumerDebt,
      equity = equityWealth,
    )

  def fixture(
      id: HhId = HhId(0),
      savings: PLN = PLN.Zero,
      debt: PLN = PLN.Zero,
      monthlyRent: PLN = PLN.Zero,
      skill: Share = Share("0.8"),
      healthPenalty: Share = Share.Zero,
      mpc: Share = Share("0.8"),
      status: HhStatus = HhStatus.Unemployed(0),
      socialNeighbors: Array[HhId] = Array.empty,
      bankId: BankId = BankId(0),
      equityWealth: PLN = PLN.Zero,
      lastSectorIdx: SectorIdx = SectorIdx(-1),
      isImmigrant: Boolean = false,
      numDependentChildren: Int = 0,
      consumerDebt: PLN = PLN.Zero,
      education: Int = 2,
      taskRoutineness: Share = Share("0.5"),
      wageScar: Share = Share.Zero,
      financialDistressMonths: Int = 0,
      contractType: ContractType = ContractType.Permanent,
      region: Region = Region.Central,
  ): Fixture =
    Fixture(
      state = apply(
        id = id,
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
      ),
      financialStocks = financial(savings = savings, debt = debt, consumerDebt = consumerDebt, equityWealth = equityWealth),
    )

  def apply(
      id: HhId = HhId(0),
      savings: PLN = PLN.Zero,
      debt: PLN = PLN.Zero,
      monthlyRent: PLN = PLN.Zero,
      skill: Share = Share("0.8"),
      healthPenalty: Share = Share.Zero,
      mpc: Share = Share("0.8"),
      status: HhStatus = HhStatus.Unemployed(0),
      socialNeighbors: Array[HhId] = Array.empty,
      bankId: BankId = BankId(0),
      equityWealth: PLN = PLN.Zero,
      lastSectorIdx: SectorIdx = SectorIdx(-1),
      isImmigrant: Boolean = false,
      numDependentChildren: Int = 0,
      consumerDebt: PLN = PLN.Zero,
      education: Int = 2,
      taskRoutineness: Share = Share("0.5"),
      wageScar: Share = Share.Zero,
      financialDistressMonths: Int = 0,
      contractType: ContractType = ContractType.Permanent,
      region: Region = Region.Central,
  ): Household.State =
    val _ = (savings, debt, equityWealth, consumerDebt)
    Household.State(
      id = id,
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
