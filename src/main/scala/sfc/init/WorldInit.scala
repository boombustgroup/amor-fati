package sfc.init

import sfc.accounting.*
import sfc.agents.*
import sfc.config.*
import sfc.engine.*
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

/** Orchestrates all initialization factories and assembles World. */
object WorldInit:

  /** Initialize a complete simulation world from a seed. Side effects: calls Config.setTotalPopulation (twice: once for
    * firm workers, once for immigrants).
    */
  def initialize(seed: Int, rc: RunConfig): InitResult =
    Random.setSeed(seed.toLong)

    // --- Firms ---
    val (firms, actualTotalPop) = FirmInit.create(Random)
    Config.setTotalPopulation(actualTotalPop)

    // --- Households ---
    var households = Household.Init.create(Random, firms)

    // --- Immigrants ---
    val (updatedHh, popIncrease) = ImmigrantInit.create(Random, households, Config.TotalPopulation)
    households = updatedHh
    if popIncrease > 0 then Config.setTotalPopulation(Config.TotalPopulation + popIncrease)

    // --- Banking sector ---
    val initConsumerLoans = households.map(_.kahanSumBy(_.consumerDebt.toDouble)).getOrElse(Config.InitConsumerLoans)
    val initBankingSector = BankInit.create(firms, households)

    // --- Demographics ---
    val initDemographics =
      if Config.DemEnabled then SocialSecurity.DemographicsState(Config.DemInitialRetirees, Config.TotalPopulation, 0)
      else if Config.ZusEnabled && Config.DemInitialRetirees > 0 then
        SocialSecurity.DemographicsState(Config.DemInitialRetirees, Config.TotalPopulation, 0)
      else SocialSecurity.DemographicsState.zero

    // --- Insurance / NBFI ---
    val initInsurance =
      if Config.InsEnabled then Insurance.initial
      else Insurance.zero
    val initNbfi =
      if Config.NbfiEnabled then Nbfi.initial
      else Nbfi.zero
    val initBondsOutstanding = PLN(Config.InitBankGovBonds + Config.InitNbpGovBonds) +
      initInsurance.govBondHoldings + initNbfi.tfiGovBondHoldings

    // --- Steady-state gross investment ---
    val initGrossInvestment =
      if Config.PhysCapEnabled then
        PLN(firms.kahanSumBy(f => (f.capitalStock * Config.PhysCapDepRates(f.sector.toInt) / 12.0).toDouble))
      else PLN.Zero
    val initGreenInvestment =
      if Config.EnergyEnabled then PLN(firms.kahanSumBy(f => (f.greenCapital * Config.GreenDepRate / 12.0).toDouble))
      else PLN.Zero

    // --- Interest rate ---
    val initRate = if rc.isEurozone then Config.EcbInitialRate else Config.NbpInitialRate

    // --- World assembly ---
    val world = World(
      0,
      Rate(0.02),
      1.0,
      GovState(
        false,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN(Config.InitGovDebt),
        PLN.Zero,
        bondsOutstanding = initBondsOutstanding,
      ),
      Nbp.State(Rate(initRate), govBondHoldings = PLN(Config.InitNbpGovBonds)),
      BankState(
        PLN(Config.InitBankLoans),
        PLN.Zero,
        PLN(Config.InitBankCapital),
        PLN(Config.InitBankDeposits),
        govBondHoldings = PLN(Config.InitBankGovBonds),
        consumerLoans = PLN(initConsumerLoans),
        corpBondHoldings = PLN(Config.CorpBondInitStock * Config.CorpBondBankShare),
      ),
      ForexState(Config.BaseExRate, PLN.Zero, PLN(Config.ExportBase), PLN.Zero, PLN.Zero),
      Household.SectorState(
        Config.TotalPopulation,
        PLN(Config.BaseWage),
        PLN(Config.BaseReservationWage),
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
      ),
      Ratio.Zero,
      Ratio.Zero,
      Config.BaseRevenue * Config.FirmsCount,
      SECTORS.map(_.sigma).toVector,
      bankingSector = initBankingSector,
      demographics = initDemographics,
      equity = if Config.GpwEnabled then
        val initEq = EquityMarket.initial
        val initHhEq =
          if Config.GpwHhEquity then
            PLN(
              Config.TotalPopulation * Config.GpwHhEquityFrac *
                Math.exp(Config.HhSavingsMu) * 0.05,
            )
          else PLN.Zero
        initEq.copy(hhEquityWealth = initHhEq)
      else EquityMarket.zero,
      housing =
        if Config.ReEnabled then HousingMarket.initial
        else HousingMarket.zero,
      gvc =
        if Config.GvcEnabled && Config.OeEnabled then GvcTrade.initial
        else GvcTrade.zero,
      expectations =
        if Config.ExpEnabled then Expectations.initial
        else Expectations.zero,
      immigration =
        if Config.ImmigEnabled then Immigration.State(Config.ImmigInitStock, 0, 0, 0.0)
        else Immigration.State.zero,
      corporateBonds = CorporateBondMarket.initial,
      insurance = initInsurance,
      nbfi = initNbfi,
      grossInvestment = initGrossInvestment,
      aggGreenInvestment = initGreenInvestment,
    )

    InitResult(world, firms, households)

  case class InitResult(world: World, firms: Array[Firm.State], households: Option[Vector[Household.State]])
