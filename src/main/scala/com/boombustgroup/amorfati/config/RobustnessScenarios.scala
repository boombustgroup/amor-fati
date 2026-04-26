package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Lightweight one-at-a-time scenario definitions for local robustness work.
  *
  * This stays in the config package because SimParams has a config-private
  * constructor/copy boundary.
  */
object RobustnessScenarios:

  enum ScenarioSet(val cliName: String):
    case Smoke extends ScenarioSet("smoke")
    case Core  extends ScenarioSet("core")

  object ScenarioSet:
    val default: ScenarioSet = ScenarioSet.Core

    def parse(value: String): Either[String, ScenarioSet] =
      values.find(_.cliName == value.trim.toLowerCase) match
        case Some(set) => Right(set)
        case None      => Left(s"--scenario-set must be one of: ${values.map(_.cliName).mkString(", ")}")

  final case class Scenario(
      id: String,
      label: String,
      category: String,
      variedParameter: String,
      variation: String,
      rationale: String,
      params: SimParams,
  )

  def scenarios(set: ScenarioSet): Vector[Scenario] =
    val baseline = SimParams.defaults

    val all = Vector(
      Scenario(
        id = "baseline",
        label = "Baseline",
        category = "baseline",
        variedParameter = "none",
        variation = "default SimParams.defaults",
        rationale = "Reference path for stochastic seed envelopes and one-at-a-time parameter comparisons.",
        params = baseline,
      ),
      Scenario(
        id = "mpc-low",
        label = "Lower household MPC",
        category = "household propensity to consume",
        variedParameter = "household.mpc",
        variation = "0.82 -> 0.72",
        rationale = "First-pass demand sensitivity to lower household consumption out of income.",
        params = baseline.copy(household = baseline.household.copy(mpc = Share(0.72))),
      ),
      Scenario(
        id = "mpc-high",
        label = "Higher household MPC",
        category = "household propensity to consume",
        variedParameter = "household.mpc",
        variation = "0.82 -> 0.90",
        rationale = "First-pass demand sensitivity to higher household consumption out of income.",
        params = baseline.copy(household = baseline.household.copy(mpc = Share(0.90))),
      ),
      Scenario(
        id = "markup-high",
        label = "Higher markup and pass-through",
        category = "firm markup/pricing",
        variedParameter = "pricing.baseMarkup, pricing.costPassthrough",
        variation = "1.15 -> 1.25, 0.40 -> 0.55",
        rationale = "Tests price-level sensitivity to firm pricing power and cost pass-through.",
        params = baseline.copy(
          pricing = baseline.pricing.copy(
            baseMarkup = Multiplier(1.25),
            costPassthrough = Coefficient(0.55),
          ),
        ),
      ),
      Scenario(
        id = "investment-fast",
        label = "Faster capital adjustment",
        category = "investment response",
        variedParameter = "capital.adjustSpeed",
        variation = "0.10 -> 0.16",
        rationale = "Tests output, imports, and balance-sheet sensitivity to faster investment adjustment.",
        params = baseline.copy(capital = baseline.capital.copy(adjustSpeed = Coefficient(0.16))),
      ),
      Scenario(
        id = "credit-tight",
        label = "Tighter credit and lower recovery",
        category = "credit/default behavior",
        variedParameter = "banking.baseSpread, banking.loanRecovery, banking.eclMigrationSensitivity",
        variation = "0.015 -> 0.030, 0.30 -> 0.20, 3.0 -> 4.0",
        rationale = "Tests financial-stability sensitivity to tighter credit and weaker default recovery.",
        params = baseline.copy(
          banking = baseline.banking.copy(
            baseSpread = Rate(0.030),
            loanRecovery = Share(0.20),
            eclMigrationSensitivity = Coefficient(4.0),
          ),
        ),
      ),
      Scenario(
        id = "fiscal-stabilizer-strong",
        label = "Stronger automatic stabilizer",
        category = "fiscal rules",
        variedParameter = "fiscal.govAutoStabMult, fiscalConsolidationSpeed55",
        variation = "3.0 -> 4.0, 0.10 -> 0.15",
        rationale = "Tests unemployment, deficit, and debt sensitivity to stronger fiscal stabilization and consolidation.",
        params = baseline.copy(
          fiscal = baseline.fiscal.copy(
            govAutoStabMult = Coefficient(4.0),
            fiscalConsolidationSpeed55 = Share(0.15),
          ),
        ),
      ),
      Scenario(
        id = "monetary-tight",
        label = "Tighter monetary stance",
        category = "policy rates",
        variedParameter = "monetary.initialRate, monetary.neutralRate, monetary.taylorAlpha",
        variation = "0.0575 -> 0.075, 0.04 -> 0.05, 1.5 -> 1.8",
        rationale = "Tests inflation, credit, debt-service, and unemployment sensitivity to tighter policy rates.",
        params = baseline.copy(
          monetary = baseline.monetary.copy(
            initialRate = Rate(0.075),
            neutralRate = Rate(0.050),
            taylorAlpha = Coefficient(1.8),
          ),
        ),
      ),
      Scenario(
        id = "external-risk-off",
        label = "External risk-off shock",
        category = "external shocks",
        variedParameter = "forex.riskOffShockMonth, forex.riskOffMagnitude, forex.irpSensitivity",
        variation = "0 -> 6, 0.10 -> 0.16, 0.15 -> 0.25",
        rationale = "Tests FX, current-account, reserves, and financial-market sensitivity to a risk-off episode.",
        params = baseline.copy(
          forex = baseline.forex.copy(
            riskOffShockMonth = 6,
            riskOffMagnitude = Share(0.16),
            irpSensitivity = Coefficient(0.25),
          ),
        ),
      ),
    )

    set match
      case ScenarioSet.Smoke => all.filter(scenario => scenario.id == "baseline" || scenario.id == "mpc-high")
      case ScenarioSet.Core  => all

end RobustnessScenarios
