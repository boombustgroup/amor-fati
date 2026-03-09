package sfc.init

import sfc.accounting.*
import sfc.agents.*
import sfc.config.*
import sfc.engine.*
import sfc.engine.markets.CorporateBondMarket
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

/** Orchestrates all initialization factories and assembles World. */
object WorldInit:

  /** Initialize a complete simulation world from a seed. */
  def initialize(seed: Int)(using p: SimParams): InitResult =
    val rng = new Random(seed.toLong)

    // --- Firms ---
    val firms = FirmInit.create(rng)
    assert(firms.length == p.pop.firmsCount)

    // --- Households ---
    val households0 = Household.Init.create(rng, firms)
    val households  = ImmigrantInit.create(rng, households0)
    val totalPop    = households.length
    assert(totalPop > 0)

    // --- Banking sector ---
    val initBankingSector = BankInit.create(firms, households)

    // --- Sub-state initializers ---
    val initDemographics = DemographicsInit.create(totalPop)
    val initInsurance    = InsuranceInit.create()
    val initNbfi         = NbfiInit.create()

    val initBondsOutstanding = p.banking.initGovBonds + p.banking.initNbpGovBonds +
      initInsurance.govBondHoldings + initNbfi.tfiGovBondHoldings

    // --- Steady-state gross investment ---
    val initGrossInvestment =
      if p.flags.physCap then PLN(firms.kahanSumBy(f => (f.capitalStock * p.capital.depRates.map(_.toDouble)(f.sector.toInt) / 12.0).toDouble))
      else PLN.Zero
    val initGreenInvestment =
      if p.flags.energy then PLN(firms.kahanSumBy(f => (f.greenCapital * p.climate.greenDepRate.toDouble / 12.0).toDouble))
      else PLN.Zero

    // --- Consumer loans aggregate (from actual HH sums) ---
    val initConsumerLoans = households.kahanSumBy(_.consumerDebt.toDouble)

    // --- World assembly ---
    val world = World(
      month = 0,
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gov = GovState(
        taxRevenue = PLN.Zero,
        deficit = PLN.Zero,
        cumulativeDebt = p.fiscal.initGovDebt,
        unempBenefitSpend = PLN.Zero,
        bondsOutstanding = initBondsOutstanding,
      ),
      nbp = Nbp.State(
        referenceRate = p.monetary.initialRate,
        govBondHoldings = p.banking.initNbpGovBonds,
        fxReserves = p.monetary.fxReserves,
      ),
      bank = BankingAggregate(
        totalLoans = p.banking.initLoans,
        nplAmount = PLN.Zero,
        capital = p.banking.initCapital,
        deposits = p.banking.initDeposits,
        govBondHoldings = p.banking.initGovBonds,
        consumerLoans = PLN(initConsumerLoans),
        consumerNpl = PLN.Zero,
        corpBondHoldings = p.corpBond.initStock * p.corpBond.bankShare.toDouble,
      ),
      forex = ForexState(
        exchangeRate = p.forex.baseExRate,
        imports = PLN.Zero,
        exports = p.forex.exportBase,
        tradeBalance = PLN.Zero,
        techImports = PLN.Zero,
      ),
      hh = Household.SectorState(
        employed = totalPop,
        marketWage = p.household.baseWage,
        reservationWage = p.household.baseReservationWage,
        totalIncome = PLN.Zero,
        consumption = PLN.Zero,
        domesticConsumption = PLN.Zero,
        importConsumption = PLN.Zero,
      ),
      automationRatio = Ratio.Zero,
      hybridRatio = Ratio.Zero,
      gdpProxy = p.firm.baseRevenue.toDouble * p.pop.firmsCount,
      currentSigmas = SectorDefs.map(_.sigma),
      bankingSector = initBankingSector,
      demographics = initDemographics,
      equity = EquityInit.create(totalPop),
      housing = HousingInit.create(),
      gvc = GvcInit.create(),
      expectations = ExpectationsInit.create(),
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
