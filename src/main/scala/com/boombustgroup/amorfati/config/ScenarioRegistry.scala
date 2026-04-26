package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Named policy and shock scenarios for reproducible experiment runs.
  *
  * This lives in the config package because scenario composition needs the
  * config-private SimParams copy boundary.
  */
object ScenarioRegistry:

  final case class ParameterDelta(
      parameter: String,
      baseline: String,
      scenario: String,
      note: String,
  )

  final case class ScenarioSpec(
      id: String,
      label: String,
      category: String,
      purpose: String,
      expectedChannels: Vector[String],
      recommendedMonths: Int,
      seedPolicy: String,
      outputFolder: String,
      deltas: Vector[ParameterDelta],
      params: SimParams,
  )

  private val Baseline = SimParams.defaults

  val defaultScenarioIds: Vector[String] =
    Vector("baseline", "monetary-tightening", "fiscal-expansion")

  val all: Vector[ScenarioSpec] =
    Vector(
      ScenarioSpec(
        id = "baseline",
        label = "Baseline",
        category = "baseline",
        purpose = "Reference scenario using SimParams.defaults.",
        expectedChannels = Vector("all baseline model channels"),
        recommendedMonths = 120,
        seedPolicy = "Use fixed seed bands; default smoke command uses seed 1.",
        outputFolder = "<out>/<run-id>/baseline",
        deltas = Vector(ParameterDelta("SimParams", "SimParams.defaults", "SimParams.defaults", "No parameter change.")),
        params = Baseline,
      ),
      ScenarioSpec(
        id = "monetary-tightening",
        label = "Monetary tightening",
        category = "policy rates",
        purpose = "Higher NBP policy stance for inflation and credit stress analysis.",
        expectedChannels = Vector("RefRate", "Inflation", "CreditToGdpGap", "DebtService", "Unemployment", "ExRate"),
        recommendedMonths = 60,
        seedPolicy = "Compare on the same seed band as baseline.",
        outputFolder = "<out>/<run-id>/monetary-tightening",
        deltas = Vector(
          ParameterDelta("monetary.initialRate", "0.0575", "0.075", "Higher starting reference rate."),
          ParameterDelta("monetary.neutralRate", "0.04", "0.05", "Higher neutral-rate anchor."),
          ParameterDelta("monetary.taylorAlpha", "1.5", "1.8", "Stronger inflation response."),
        ),
        params = Baseline.copy(
          monetary = Baseline.monetary.copy(
            initialRate = Rate.decimal(75, 3),
            neutralRate = Rate.decimal(50, 3),
            taylorAlpha = Coefficient.decimal(18, 1),
          ),
        ),
      ),
      ScenarioSpec(
        id = "fiscal-expansion",
        label = "Fiscal expansion",
        category = "fiscal policy",
        purpose = "Higher government demand and capital share for fiscal multiplier and debt-path comparisons.",
        expectedChannels = Vector("GovCurrentSpend", "GovCapitalSpendDomestic", "DeficitToGdp", "DebtToGdp", "Unemployment", "Inflation"),
        recommendedMonths = 60,
        seedPolicy = "Compare on the same seed band as baseline.",
        outputFolder = "<out>/<run-id>/fiscal-expansion",
        deltas = Vector(
          ParameterDelta("fiscal.govBaseSpending", "scaled default", "scaled default * 1.15", "15% higher base government spending."),
          ParameterDelta("fiscal.govInvestShare", "0.20", "0.30", "Higher capital-spending share."),
          ParameterDelta("fiscal.govAutoStabMult", "3.0", "3.5", "Stronger automatic stabilization."),
        ),
        params = Baseline.copy(
          fiscal = Baseline.fiscal.copy(
            govBaseSpending = Baseline.fiscal.govBaseSpending * Multiplier.decimal(115, 2),
            govInvestShare = Share.decimal(30, 2),
            govAutoStabMult = Coefficient.decimal(35, 1),
          ),
        ),
      ),
      ScenarioSpec(
        id = "credit-crunch",
        label = "Credit crunch",
        category = "banking stress",
        purpose = "Tighter credit supply and weaker recovery assumptions for credit and default stress testing.",
        expectedChannels = Vector("CreditToGdpGap", "ConsumerLoans", "MinBankCAR", "MaxBankNPL", "FirmDeaths", "Unemployment"),
        recommendedMonths = 60,
        seedPolicy = "Compare on the same seed band as baseline.",
        outputFolder = "<out>/<run-id>/credit-crunch",
        deltas = Vector(
          ParameterDelta("banking.baseSpread", "0.015", "0.035", "Higher firm-loan spread."),
          ParameterDelta("banking.minCar", "0.08", "0.10", "Higher capital constraint."),
          ParameterDelta("banking.loanRecovery", "0.30", "0.20", "Lower corporate-loan recovery."),
          ParameterDelta("banking.eclMigrationSensitivity", "3.0", "4.5", "Faster IFRS 9 migration under stress."),
          ParameterDelta("household.ccMaxDti", "0.40", "0.30", "Tighter household credit affordability cap."),
        ),
        params = Baseline.copy(
          banking = Baseline.banking.copy(
            baseSpread = Rate.decimal(35, 3),
            minCar = Multiplier.decimal(10, 2),
            loanRecovery = Share.decimal(20, 2),
            eclMigrationSensitivity = Coefficient.decimal(45, 1),
          ),
          household = Baseline.household.copy(ccMaxDti = Share.decimal(30, 2)),
        ),
      ),
      ScenarioSpec(
        id = "energy-shock",
        label = "Energy shock",
        category = "climate and commodity shock",
        purpose = "ETS and commodity-price stress for import costs, inflation, green capital, and sector pressure.",
        expectedChannels = Vector("CommodityPriceIndex", "GvcImportCostIndex", "AggEnergyCost", "Inflation", "CurrentAccount", "GreenInvestment"),
        recommendedMonths = 60,
        seedPolicy = "Compare on the same seed band as baseline; shock starts at month 6.",
        outputFolder = "<out>/<run-id>/energy-shock",
        deltas = Vector(
          ParameterDelta("climate.etsBasePrice", "80", "120", "Higher EU ETS starting price."),
          ParameterDelta(
            "climate.energyCostShares",
            "[0.02,0.10,0.04,0.05,0.03,0.06]",
            "[0.03,0.15,0.06,0.075,0.045,0.09]",
            "50% higher sector energy-cost burden.",
          ),
          ParameterDelta("climate.greenBudgetShare", "0.20", "0.12", "Lower discretionary green investment capacity under stress."),
          ParameterDelta("gvc.commodityShockMonth", "0", "6", "Commodity-price shock starts in month 6."),
          ParameterDelta("gvc.commodityShockMag", "0.0", "1.5", "One-time commodity-price shock magnitude."),
        ),
        params = Baseline.copy(
          climate = Baseline.climate.copy(
            etsBasePrice = Multiplier(120),
            energyCostShares = Vector(
              Share.decimal(3, 2),
              Share.decimal(15, 2),
              Share.decimal(6, 2),
              Share.decimal(75, 3),
              Share.decimal(45, 3),
              Share.decimal(9, 2),
            ),
            greenBudgetShare = Share.decimal(12, 2),
          ),
          gvc = Baseline.gvc.copy(
            commodityShockMonth = 6,
            commodityShockMag = Multiplier.decimal(15, 1),
          ),
        ),
      ),
      ScenarioSpec(
        id = "tourism-shock",
        label = "Tourism shock",
        category = "external demand shock",
        purpose = "Tourism-demand loss for services demand, current account, employment, and FX flows.",
        expectedChannels = Vector("TourismExport", "TourismImport", "NetTourismBalance", "CurrentAccount", "Unemployment", "Inflation"),
        recommendedMonths = 36,
        seedPolicy = "Compare on the same seed band as baseline; shock starts at month 6.",
        outputFolder = "<out>/<run-id>/tourism-shock",
        deltas = Vector(
          ParameterDelta("tourism.shockMonth", "0", "6", "Tourism shock starts in month 6."),
          ParameterDelta("tourism.shockSize", "0.80", "0.60", "60% tourism-demand loss in the named scenario."),
          ParameterDelta("tourism.shockRecovery", "0.03", "0.05", "Faster monthly recovery than default COVID-style setting."),
          ParameterDelta("tourism.inboundShare", "0.05", "0.04", "Lower inbound tourism baseline share."),
        ),
        params = Baseline.copy(
          tourism = Baseline.tourism.copy(
            shockMonth = 6,
            shockSize = Share.decimal(60, 2),
            shockRecovery = Rate.decimal(5, 2),
            inboundShare = Share.decimal(4, 2),
          ),
        ),
      ),
      ScenarioSpec(
        id = "bank-failure",
        label = "Bank failure stress",
        category = "financial stability",
        purpose = "Low bank-capital and deposit-panic stress for failures, liquidity, and resolution channels.",
        expectedChannels = Vector("BankFailures", "MinBankCAR", "MinBankLCR", "BfgFundBalance", "BailInLoss", "NPL"),
        recommendedMonths = 36,
        seedPolicy = "Compare on the same seed band as baseline.",
        outputFolder = "<out>/<run-id>/bank-failure",
        deltas = Vector(
          ParameterDelta("banking.initCapital", "scaled default", "scaled default * 0.55", "Lower opening banking-sector capital."),
          ParameterDelta("banking.minCar", "0.08", "0.12", "Higher regulatory capital requirement."),
          ParameterDelta("banking.depositPanicRate", "0.03", "0.08", "Higher deposit panic migration after failures."),
          ParameterDelta("banking.maxDepositSwitchRate", "0.10", "0.18", "Higher maximum monthly deposit switching."),
        ),
        params = Baseline.copy(
          banking = Baseline.banking.copy(
            initCapital = Baseline.banking.initCapital * Multiplier.decimal(55, 2),
            minCar = Multiplier.decimal(12, 2),
            depositPanicRate = Share.decimal(8, 2),
            maxDepositSwitchRate = Share.decimal(18, 2),
          ),
        ),
      ),
      ScenarioSpec(
        id = "fx-capital-flight",
        label = "FX and capital-flight stress",
        category = "external financial shock",
        purpose = "Risk-off and carry-unwind stress for exchange rate, reserves, current account, and bond demand.",
        expectedChannels = Vector("ExRate", "FxReserves", "FxInterventionAmt", "CurrentAccount", "ForeignBondHoldings", "BondYield"),
        recommendedMonths = 60,
        seedPolicy = "Compare on the same seed band as baseline; shock starts at month 6.",
        outputFolder = "<out>/<run-id>/fx-capital-flight",
        deltas = Vector(
          ParameterDelta("forex.riskOffShockMonth", "0", "6", "Risk-off shock starts in month 6."),
          ParameterDelta("forex.riskOffMagnitude", "0.10", "0.20", "Larger capital outflow shock."),
          ParameterDelta("forex.riskOffDurationMonths", "6", "9", "Longer elevated risk-off period."),
          ParameterDelta("forex.irpSensitivity", "0.15", "0.30", "Stronger exchange-rate response to rate differentials."),
          ParameterDelta("forex.exRateAdjSpeed", "0.02", "0.05", "Faster exchange-rate adjustment."),
          ParameterDelta("monetary.fxMaxMonthly", "0.03", "0.06", "Larger allowed monthly FX intervention."),
        ),
        params = Baseline.copy(
          forex = Baseline.forex.copy(
            riskOffShockMonth = 6,
            riskOffMagnitude = Share.decimal(20, 2),
            riskOffDurationMonths = 9,
            irpSensitivity = Coefficient.decimal(30, 2),
            exRateAdjSpeed = Coefficient.decimal(5, 2),
          ),
          monetary = Baseline.monetary.copy(fxMaxMonthly = Share.decimal(6, 2)),
        ),
      ),
      ScenarioSpec(
        id = "quasi-fiscal-program",
        label = "Quasi-fiscal program",
        category = "quasi-fiscal policy",
        purpose = "BGK/PFR-style off-budget financing stress for ESA debt, NBP absorption, and subsidized lending.",
        expectedChannels = Vector("QfBondsOutstanding", "QfIssuance", "QfLoanPortfolio", "QfNbpHoldings", "Esa2010DebtToGdp", "DebtToGdp"),
        recommendedMonths = 60,
        seedPolicy = "Compare on the same seed band as baseline.",
        outputFolder = "<out>/<run-id>/quasi-fiscal-program",
        deltas = Vector(
          ParameterDelta("quasiFiscal.issuanceShare", "0.40", "0.65", "Higher BGK/PFR share of capital programs."),
          ParameterDelta("quasiFiscal.lendingShare", "0.50", "0.70", "More issuance routed to subsidized lending."),
          ParameterDelta("quasiFiscal.nbpAbsorptionShare", "0.70", "0.85", "Higher NBP absorption when QE is active."),
          ParameterDelta("fiscal.govInvestShare", "0.20", "0.30", "Higher capital-spending share feeding quasi-fiscal issuance."),
          ParameterDelta("monetary.qeMaxGdpShare", "0.30", "0.40", "Higher QE stock ceiling."),
        ),
        params = Baseline.copy(
          quasiFiscal = Baseline.quasiFiscal.copy(
            issuanceShare = Share.decimal(65, 2),
            lendingShare = Share.decimal(70, 2),
            nbpAbsorptionShare = Share.decimal(85, 2),
          ),
          fiscal = Baseline.fiscal.copy(govInvestShare = Share.decimal(30, 2)),
          monetary = Baseline.monetary.copy(qeMaxGdpShare = Share.decimal(40, 2)),
        ),
      ),
    )

  private val byId: Map[String, ScenarioSpec] = all.map(scenario => scenario.id -> scenario).toMap

  def get(id: String): Either[String, ScenarioSpec] =
    byId.get(id).toRight(s"Unknown scenario '$id'. Known scenarios: ${all.map(_.id).mkString(", ")}")

  def select(value: String): Either[String, Vector[ScenarioSpec]] =
    val normalized = value.trim
    if normalized == "all" then Right(all)
    else
      val ids = normalized.split(",").iterator.map(_.trim).filter(_.nonEmpty).toVector
      if ids.isEmpty then Left("--scenarios must contain at least one scenario id")
      else
        ids.foldLeft[Either[String, Vector[ScenarioSpec]]](Right(Vector.empty)): (acc, id) =>
          for
            selected <- acc
            scenario <- get(id)
          yield selected :+ scenario

end ScenarioRegistry
