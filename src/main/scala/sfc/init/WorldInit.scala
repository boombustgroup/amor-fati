package sfc.init

import sfc.accounting.*
import sfc.agents.*
import sfc.config.*
import sfc.engine.*
import sfc.engine.markets.{CorporateBondMarket, EquityMarket, GvcTrade, HousingMarket}
import sfc.engine.mechanisms.Expectations
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

/** Orchestrates all initialization factories and assembles World. */
object WorldInit:

  /** Initialize a complete simulation world from a seed. Sets totalPopulation
    * (twice: once for firm workers, once for immigrants).
    */
  def initialize(seed: Int, rc: RunConfig)(using p: SimParams): InitResult =
    Random.setSeed(seed.toLong)

    // --- Firms ---
    val (firms, actualTotalPop) = FirmInit.create(Random)
    var totalPop                = actualTotalPop

    // --- Households ---
    var households = Household.Init.create(Random, firms)

    // --- Immigrants ---
    val (updatedHh, popIncrease) = ImmigrantInit.create(Random, households, totalPop)
    households = updatedHh
    if popIncrease > 0 then totalPop = totalPop + popIncrease

    // --- Banking sector ---
    val initConsumerLoans = households.kahanSumBy(_.consumerDebt.toDouble)
    val initBankingSector = BankInit.create(firms, households)

    // --- Demographics ---
    val initDemographics =
      if p.flags.demographics then SocialSecurity.DemographicsState(p.social.demInitialRetirees, totalPop, 0)
      else if p.flags.zus && p.social.demInitialRetirees > 0 then SocialSecurity.DemographicsState(p.social.demInitialRetirees, totalPop, 0)
      else SocialSecurity.DemographicsState.zero

    // --- Insurance / NBFI ---
    val initInsurance        =
      if p.flags.insurance then Insurance.initial
      else Insurance.zero
    val initNbfi             =
      if p.flags.nbfi then Nbfi.initial
      else Nbfi.zero
    val initBondsOutstanding = PLN(p.banking.initGovBonds.toDouble + p.banking.initNbpGovBonds.toDouble) +
      initInsurance.govBondHoldings + initNbfi.tfiGovBondHoldings

    // --- Steady-state gross investment ---
    val initGrossInvestment =
      if p.flags.physCap then
        PLN(
          firms.kahanSumBy(f => (f.capitalStock * p.capital.depRates.map(_.toDouble)(f.sector.toInt) / 12.0).toDouble),
        )
      else PLN.Zero
    val initGreenInvestment =
      if p.flags.energy then PLN(firms.kahanSumBy(f => (f.greenCapital * p.climate.greenDepRate.toDouble / 12.0).toDouble))
      else PLN.Zero

    // --- Interest rate ---
    val initRate = p.monetary.initialRate.toDouble

    // --- World assembly ---
    val world = World(
      0,
      Rate(0.02),
      1.0,
      GovState(
        PLN.Zero,
        PLN.Zero,
        PLN(p.fiscal.initGovDebt.toDouble),
        PLN.Zero,
        bondsOutstanding = initBondsOutstanding,
      ),
      Nbp.State(
        Rate(initRate),
        govBondHoldings = PLN(p.banking.initNbpGovBonds.toDouble),
        fxReserves = PLN(p.monetary.fxReserves.toDouble),
      ),
      BankingAggregate(
        PLN(p.banking.initLoans.toDouble),
        PLN.Zero,
        PLN(p.banking.initCapital.toDouble),
        PLN(p.banking.initDeposits.toDouble),
        govBondHoldings = PLN(p.banking.initGovBonds.toDouble),
        consumerLoans = PLN(initConsumerLoans),
        consumerNpl = PLN.Zero,
        corpBondHoldings = PLN(p.corpBond.initStock.toDouble * p.corpBond.bankShare.toDouble),
      ),
      ForexState(p.forex.baseExRate, PLN.Zero, PLN(p.forex.exportBase.toDouble), PLN.Zero, PLN.Zero),
      Household.SectorState(
        totalPop,
        PLN(p.household.baseWage.toDouble),
        PLN(p.household.baseReservationWage.toDouble),
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
      ),
      Ratio.Zero,
      Ratio.Zero,
      p.firm.baseRevenue.toDouble * p.pop.firmsCount,
      SectorDefs.map(_.sigma).toVector,
      bankingSector = initBankingSector,
      demographics = initDemographics,
      equity = if p.flags.gpw then
        val initEq   = EquityMarket.initial
        val initHhEq =
          if p.flags.gpwHhEquity then
            PLN(
              totalPop * p.equity.hhEquityFrac.toDouble *
                Math.exp(p.household.savingsMu) * 0.05,
            )
          else PLN.Zero
        initEq.copy(hhEquityWealth = initHhEq)
      else EquityMarket.zero,
      housing =
        if p.flags.re then HousingMarket.initial
        else HousingMarket.zero,
      gvc =
        if p.flags.gvc && p.flags.openEcon then GvcTrade.initial
        else GvcTrade.zero,
      expectations =
        if p.flags.expectations then Expectations.initial
        else Expectations.zero,
      immigration =
        if p.flags.immigration then Immigration.State(p.immigration.initStock, 0, 0, 0.0)
        else Immigration.State.zero,
      corporateBonds = CorporateBondMarket.initial,
      insurance = initInsurance,
      nbfi = initNbfi,
      grossInvestment = initGrossInvestment,
      aggGreenInvestment = initGreenInvestment,
      totalPopulation = totalPop,
    )

    InitResult(world, firms, households)

  case class InitResult(world: World, firms: Vector[Firm.State], households: Vector[Household.State])
