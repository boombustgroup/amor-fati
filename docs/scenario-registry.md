# Scenario Registry

This registry defines named policy and shock experiments that can be inspected,
run, and compared reproducibly. It is separate from the sensitivity workflow:
scenario runs are named economic stories, while sensitivity runs are local
one-at-a-time parameter checks.

The source of truth is
`src/main/scala/com/boombustgroup/amorfati/config/ScenarioRegistry.scala`.
The executable runner is
`src/main/scala/com/boombustgroup/amorfati/diagnostics/ScenarioRunExport.scala`.

## Command

Run the baseline plus two nontrivial scenarios:

```bash
sbt "scenarioRun --scenarios baseline,monetary-tightening,fiscal-expansion --seeds 1 --months 12 --run-id scenario-smoke --out target/scenarios"
```

Run every registered scenario:

```bash
sbt "scenarioRun --scenarios all --seeds 2 --months 24 --run-id local-review --out target/scenarios"
```

Equivalent direct entrypoint:

```bash
sbt "runMain com.boombustgroup.amorfati.diagnostics.ScenarioRunExport --scenarios all --seeds 2 --months 24 --run-id local-review --out target/scenarios"
```

## Output Layout

For `--out target/scenarios --run-id local-review`, the runner writes:

```text
target/scenarios/local-review/
  scenario-registry.md
  scenario-deltas.csv
  run-summary.csv
  baseline/
    metadata.md
    local-review_baseline_24m_seed001.csv
  monetary-tightening/
    metadata.md
    local-review_monetary-tightening_24m_seed001.csv
```

Each per-seed CSV uses `McTimeseriesSchema.colNames` as the header, so scenario
outputs can be compared directly with validation and robustness artifacts.

## Registered Scenarios

| Scenario | Label | Category | Recommended months | Purpose |
| --- | --- | --- | --- | --- |
| `baseline` | Baseline | baseline | 120 | Reference scenario using `SimParams.defaults`. |
| `monetary-tightening` | Monetary tightening | policy rates | 60 | Higher NBP policy stance for inflation and credit stress analysis. |
| `fiscal-expansion` | Fiscal expansion | fiscal policy | 60 | Higher government demand and capital share for fiscal multiplier and debt-path comparisons. |
| `credit-crunch` | Credit crunch | banking stress | 60 | Tighter credit supply and weaker recovery assumptions for credit and default stress testing. |
| `energy-shock` | Energy shock | climate and commodity shock | 60 | ETS and commodity-price stress for import costs, inflation, green capital, and sector pressure. |
| `tourism-shock` | Tourism shock | external demand shock | 36 | Tourism-demand loss for services demand, current account, employment, and FX flows. |
| `bank-failure` | Bank failure stress | financial stability | 36 | Low bank-capital and deposit-panic stress for failures, liquidity, and resolution channels. |
| `fx-capital-flight` | FX and capital-flight stress | external financial shock | 60 | Risk-off and carry-unwind stress for exchange rate, reserves, current account, and bond demand. |
| `quasi-fiscal-program` | Quasi-fiscal program | quasi-fiscal policy | 60 | BGK/PFR-style off-budget financing stress for ESA debt, NBP absorption, and subsidized lending. |

## Parameter Deltas

| Scenario | Parameter | Baseline | Scenario value | Note |
| --- | --- | --- | --- | --- |
| `baseline` | `SimParams` | `SimParams.defaults` | `SimParams.defaults` | No parameter change. |
| `monetary-tightening` | `monetary.initialRate` | `0.0575` | `0.075` | Higher starting reference rate. |
| `monetary-tightening` | `monetary.neutralRate` | `0.04` | `0.05` | Higher neutral-rate anchor. |
| `monetary-tightening` | `monetary.taylorAlpha` | `1.5` | `1.8` | Stronger inflation response. |
| `fiscal-expansion` | `fiscal.govBaseSpending` | scaled default | scaled default `* 1.15` | 15% higher base government spending. |
| `fiscal-expansion` | `fiscal.govInvestShare` | `0.20` | `0.30` | Higher capital-spending share. |
| `fiscal-expansion` | `fiscal.govAutoStabMult` | `3.0` | `3.5` | Stronger automatic stabilization. |
| `credit-crunch` | `banking.baseSpread` | `0.015` | `0.035` | Higher firm-loan spread. |
| `credit-crunch` | `banking.minCar` | `0.08` | `0.10` | Higher capital constraint. |
| `credit-crunch` | `banking.loanRecovery` | `0.30` | `0.20` | Lower corporate-loan recovery. |
| `credit-crunch` | `banking.eclMigrationSensitivity` | `3.0` | `4.5` | Faster IFRS 9 migration under stress. |
| `credit-crunch` | `household.ccMaxDti` | `0.40` | `0.30` | Tighter household credit affordability cap. |
| `energy-shock` | `climate.etsBasePrice` | `80` | `120` | Higher EU ETS starting price. |
| `energy-shock` | `climate.energyCostShares` | `[0.02,0.10,0.04,0.05,0.03,0.06]` | `[0.03,0.15,0.06,0.075,0.045,0.09]` | 50% higher sector energy-cost burden. |
| `energy-shock` | `climate.greenBudgetShare` | `0.20` | `0.12` | Lower discretionary green investment capacity under stress. |
| `energy-shock` | `gvc.commodityShockMonth` | `0` | `6` | Commodity-price shock starts in month 6. |
| `energy-shock` | `gvc.commodityShockMag` | `0.0` | `1.5` | One-time commodity-price shock magnitude. |
| `tourism-shock` | `tourism.shockMonth` | `0` | `6` | Tourism shock starts in month 6. |
| `tourism-shock` | `tourism.shockSize` | `0.80` | `0.60` | 60% tourism-demand loss in the named scenario. |
| `tourism-shock` | `tourism.shockRecovery` | `0.03` | `0.05` | Faster monthly recovery than default COVID-style setting. |
| `tourism-shock` | `tourism.inboundShare` | `0.05` | `0.04` | Lower inbound tourism baseline share. |
| `bank-failure` | `banking.initCapital` | scaled default | scaled default `* 0.55` | Lower opening banking-sector capital. |
| `bank-failure` | `banking.minCar` | `0.08` | `0.12` | Higher regulatory capital requirement. |
| `bank-failure` | `banking.depositPanicRate` | `0.03` | `0.08` | Higher deposit panic migration after failures. |
| `bank-failure` | `banking.maxDepositSwitchRate` | `0.10` | `0.18` | Higher maximum monthly deposit switching. |
| `fx-capital-flight` | `forex.riskOffShockMonth` | `0` | `6` | Risk-off shock starts in month 6. |
| `fx-capital-flight` | `forex.riskOffMagnitude` | `0.10` | `0.20` | Larger capital outflow shock. |
| `fx-capital-flight` | `forex.riskOffDurationMonths` | `6` | `9` | Longer elevated risk-off period. |
| `fx-capital-flight` | `forex.irpSensitivity` | `0.15` | `0.30` | Stronger exchange-rate response to rate differentials. |
| `fx-capital-flight` | `forex.exRateAdjSpeed` | `0.02` | `0.05` | Faster exchange-rate adjustment. |
| `fx-capital-flight` | `monetary.fxMaxMonthly` | `0.03` | `0.06` | Larger allowed monthly FX intervention. |
| `quasi-fiscal-program` | `quasiFiscal.issuanceShare` | `0.40` | `0.65` | Higher BGK/PFR share of capital programs. |
| `quasi-fiscal-program` | `quasiFiscal.lendingShare` | `0.50` | `0.70` | More issuance routed to subsidized lending. |
| `quasi-fiscal-program` | `quasiFiscal.nbpAbsorptionShare` | `0.70` | `0.85` | Higher NBP absorption when QE is active. |
| `quasi-fiscal-program` | `fiscal.govInvestShare` | `0.20` | `0.30` | Higher capital-spending share feeding quasi-fiscal issuance. |
| `quasi-fiscal-program` | `monetary.qeMaxGdpShare` | `0.30` | `0.40` | Higher QE stock ceiling. |

## Links To Other Research Artifacts

- [docs/empirical-validation-report.md](empirical-validation-report.md):
  scenario outputs reuse the same Monte Carlo columns and validation targets.
- [docs/sensitivity-robustness-workflow.md](sensitivity-robustness-workflow.md):
  robustness runs can wrap named scenarios later, but the current sensitivity
  workflow remains local and one-at-a-time.
- [docs/calibration-register.md](calibration-register.md): baseline parameter
  provenance remains there; this document records scenario deltas from that
  baseline.
- [docs/data-bridge-national-financial-accounts.md](data-bridge-national-financial-accounts.md):
  official source mapping and transformation rules for scenario inputs that are
  empirical or based on historical analogues.
