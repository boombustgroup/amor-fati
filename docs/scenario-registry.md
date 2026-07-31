# Scenario Registry

This registry defines named policy and shock experiments that can be inspected,
run, and compared reproducibly. It is separate from the sensitivity workflow:
scenario runs are named economic stories, while sensitivity runs are local
one-at-a-time parameter checks.

The source of truth is
`modules/model/src/main/scala/com/boombustgroup/amorfati/config/ScenarioRegistry.scala`.
The executable runner is
`modules/cli/src/main/scala/com/boombustgroup/amorfati/diagnostics/ScenarioRunExport.scala`.

Operational appendix entry point:
[operations.md#operational-appendix-index](operations.md#operational-appendix-index).
Use this page for named scenario execution details after starting from the
operations index.

## Command

Run the resolved baseline plus two nontrivial scenario patches:

```bash
sbt "scenarioRun --baseline PL-2026-04-30-legacy-v1 --scenarios monetary-tightening,fiscal-expansion --seeds 1 --months 12 --run-id scenario-smoke --out target/scenarios"
```

Run every registered scenario:

```bash
sbt "scenarioRun --baseline PL-2026-04-30-legacy-v1 --scenarios all --seeds 2 --months 24 --run-id local-review --out target/scenarios"
```

Equivalent direct entrypoint:

```bash
sbt "runMain com.boombustgroup.amorfati.diagnostics.ScenarioRunExport --baseline PL-2026-04-30-legacy-v1 --scenarios all --seeds 2 --months 24 --run-id local-review --out target/scenarios"
```

## Output Layout

For `--out target/scenarios --run-id local-review`, the runner writes:

```text
target/scenarios/local-review/
  scenario-registry.md
  scenario-deltas.tsv
  run-summary.tsv
  baseline/
    metadata.md
    local-review_baseline_24m_seed001.tsv
  monetary-tightening/
    metadata.md
    local-review_monetary-tightening_24m_seed001.tsv
```

Each invocation first writes an unpatched run for the resolved baseline, then
writes one run for each selected scenario patch. Each per-seed TSV uses
`McTimeseriesSchema.colNames` as the header, so outputs can be compared
directly with validation and robustness artifacts.

The generated registry and per-scenario metadata are code-backed by
`ScenarioRegistry`. `scenario-deltas.tsv` includes the parameter diff plus
stable `ProvenanceClassification` ids, `SourceProvider`, `Vintage`, and
`TransformationNotes` for each delta. The generated markdown files render
human-readable provenance labels and repeat the per-delta transformation notes.

## Provenance Contract

`ScenarioRegistry.ParameterDelta` carries `DeltaProvenance` for every registered
delta. The allowed `ProvenanceClassification` values are:

| Classification | Meaning |
| --- | --- |
| `historical_analogue` | Stress magnitude or timing is based on an analogue event or stress pattern. |
| `official_projection` | Scenario input follows an official forecast or projection path. |
| `policy_counterfactual` | Scenario input is a deliberate policy alternative to the baseline. |
| `explicit_assumption` | Scenario input is a documented modeling assumption without external-path backing. |

## Registered Scenarios

The baseline is not a scenario. It is selected by `--baseline`; the current
registry's patches declare compatibility only with
`PL-2026-04-30-legacy-v1` until a separately versioned patch set is prepared
for another baseline.

| Scenario | Label | Category | Provenance | Source/provider | Vintage | Recommended months | Purpose |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `monetary-tightening` | Monetary tightening | policy rates | policy counterfactual | Internal policy scenario over the NBP rate channel | 2026-04-30 baseline counterfactual | 60 | Higher NBP policy stance for inflation and credit stress analysis. |
| `fiscal-expansion` | Fiscal expansion | fiscal policy | policy counterfactual | Internal fiscal policy scenario | 2026-04-30 baseline counterfactual | 60 | Higher government demand and capital share for fiscal multiplier and debt-path comparisons. |
| `credit-crunch` | Credit crunch | banking stress | historical analogue | Internal banking-stress analogue | post-2008 and pandemic credit-stress analogue | 60 | Tighter credit supply and weaker recovery assumptions for credit and default stress testing. |
| `energy-shock` | Energy shock | climate and commodity shock | historical analogue | EU ETS and commodity-price stress analogue | 2021-2022 European energy shock analogue | 60 | ETS and commodity-price stress for import costs, inflation, green capital, and sector pressure. |
| `tourism-shock` | Tourism shock | external demand shock | historical analogue | COVID-era tourism demand loss analogue | 2020-2021 tourism shock analogue | 36 | Tourism-demand loss for services demand, current account, employment, and FX flows. |
| `bank-failure` | Bank failure stress | financial stability | historical analogue | Internal bank-run and resolution stress analogue | historical bank-run stress analogue | 36 | Low bank-capital and deposit-panic stress for failures, liquidity, and resolution channels. |
| `fx-capital-flight` | FX and capital-flight stress | external financial shock | historical analogue | Emerging-market risk-off stress analogue | 2013 taper-tantrum and 2022 tightening analogue | 60 | Risk-off and carry-unwind stress for exchange rate, reserves, current account, and bond demand. |
| `quasi-fiscal-program` | Quasi-fiscal program | quasi-fiscal policy | policy counterfactual | Internal BGK/PFR-style quasi-fiscal policy scenario | 2026-04-30 baseline counterfactual | 60 | BGK/PFR-style off-budget financing stress for ESA debt, NBP absorption, and subsidized lending. |

## Parameter Deltas

The compact table below records each patch relative to the resolved baseline.
The generated
`scenario-deltas.tsv` appends provenance classification, source/provider,
vintage, and transformation notes from the same `ScenarioRegistry` delta
objects.

| Scenario | Parameter | Baseline | Scenario value | Note |
| --- | --- | --- | --- | --- |
| `monetary-tightening` | `monetary.initialRate` | `0.0375` | `0.075` | Higher starting reference rate. |
| `monetary-tightening` | `monetary.neutralRate` | `0.04` | `0.05` | Higher neutral-rate anchor. |
| `monetary-tightening` | `monetary.taylorAlpha` | `1.5` | `1.8` | Stronger inflation response. |
| `fiscal-expansion` | `fiscal.govBaseSpending` | scaled default | scaled default $\cdot 1.15$ | 15% higher base government spending. |
| `fiscal-expansion` | `fiscal.govInvestShare` | `0.20` | `0.30` | Higher capital-spending share. |
| `fiscal-expansion` | `fiscal.govAutoStabMult` | `3.0` | `3.5` | Stronger automatic stabilization. |
| `credit-crunch` | `banking.baseSpread` | `0.015` | `0.035` | Higher firm-loan spread. |
| `credit-crunch` | `banking.minCar` | `0.08` | `0.10` | Higher capital constraint. |
| `credit-crunch` | `banking.loanRecovery` | `0.30` | `0.20` | Lower corporate-loan recovery. |
| `credit-crunch` | `banking.eclMigrationSensitivity` | `3.0` | `4.5` | Faster IFRS 9 migration under stress. |
| `credit-crunch` | `household.ccMaxDti` | `0.40` | `0.30` | Tighter household credit affordability cap. |
| `credit-crunch` | `household.ccEligRate` | `0.85` | `0.30` | Lower stressed-household access to underwritten consumer credit. |
| `energy-shock` | `climate.etsBasePrice` | `80` | `120` | Higher EU ETS starting price. |
| `energy-shock` | `climate.energyCostShares` | `[0.02,0.10,0.04,0.05,0.03,0.06]` | `[0.03,0.15,0.06,0.075,0.045,0.09]` | 50% higher sector energy-cost burden. |
| `energy-shock` | `climate.greenBudgetShare` | `0.20` | `0.12` | Lower discretionary green investment capacity under stress. |
| `energy-shock` | `gvc.commodityShockMonth` | `0` | `6` | Commodity-price shock starts in month 6. |
| `energy-shock` | `gvc.commodityShockMag` | `0.0` | `1.5` | One-time commodity-price shock magnitude. |
| `tourism-shock` | `tourism.shockMonth` | `0` | `6` | Tourism shock starts in month 6. |
| `tourism-shock` | `tourism.shockSize` | `0.80` | `0.60` | 60% tourism-demand loss in the named scenario. |
| `tourism-shock` | `tourism.shockRecovery` | `0.03` | `0.05` | Faster monthly recovery than default COVID-style setting. |
| `tourism-shock` | `tourism.inboundShare` | `0.05` | `0.04` | Lower inbound tourism baseline share. |
| `bank-failure` | `banking.openingBankProfileScenario` | baseline profile | `haircutOwnFunds(0.55)` | Applies a 45% own-funds haircut across the explicit opening bank profile. |
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
  empirical or based on external analogues.
