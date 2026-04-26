# Sensitivity And Robustness Workflow

This workflow is the first lightweight reproducible path for seed uncertainty
and one-at-a-time parameter sensitivity around Amor Fati outputs. It builds on
the existing deterministic Monte Carlo runner and the output schema documented
in `McTimeseriesSchema`.

The workflow deliberately separates two questions:

| Surface | Question | Artifact |
| --- | --- | --- |
| Stochastic uncertainty | How much does the baseline move across seed bands? | Baseline rows in `envelope-summary.csv` and `robustness-report.md` |
| Parameter sensitivity | How much does a selected one-at-a-time parameter change move terminal means versus baseline? | `sensitivity-summary.csv` and the parameter sensitivity section of `robustness-report.md` |

This is not a full global sensitivity design. It is a local research-review
workflow that should stay cheap enough to run before larger empirical and
scenario work.

## Command

Local review default:

```bash
sbt "robustnessReport --scenario-set core --seeds 2 --months 24 --out target/robustness"
```

Fast smoke check:

```bash
sbt "robustnessReport --scenario-set smoke --seeds 1 --months 6 --out target/robustness-smoke"
```

Equivalent direct entrypoint:

```bash
sbt "runMain com.boombustgroup.amorfati.diagnostics.SensitivityRobustnessExport --scenario-set core --seeds 2 --months 24 --out target/robustness"
```

## Output Artifacts

The workflow writes all generated artifacts under the selected `--out`
directory:

| File | Purpose |
| --- | --- |
| `seed-metrics.csv` | Per scenario, seed, and metric terminal value plus path min/max/mean. |
| `envelope-summary.csv` | Cross-seed envelopes by scenario and metric. |
| `sensitivity-summary.csv` | Terminal mean deltas for each non-baseline scenario versus the baseline mean. |
| `robustness-report.md` | Human-readable report separating stochastic uncertainty from parameter sensitivity. |

These files are generated outputs and should normally remain under `target/`.

## Scenario Sets

| Scenario set | Contents | Intended use |
| --- | --- | --- |
| `smoke` | `baseline`, `mpc-high` | Fast CI/local sanity check that the workflow runs and writes artifacts. |
| `core` | `baseline`, `mpc-low`, `mpc-high`, `markup-high`, `investment-fast`, `credit-tight`, `fiscal-stabilizer-strong`, `monetary-tight`, `external-risk-off` | First-pass local robustness review. |

The core set covers the first parameters most likely to matter for early SFC-ABM
review:

| Category | Scenario | Parameter deltas |
| --- | --- | --- |
| Household propensity to consume | `mpc-low`, `mpc-high` | `household.mpc` around the baseline `0.82`. |
| Firm markup/pricing | `markup-high` | Higher `pricing.baseMarkup` and `pricing.costPassthrough`. |
| Investment response | `investment-fast` | Higher `capital.adjustSpeed`. |
| Credit/default behavior | `credit-tight` | Higher `banking.baseSpread`, lower `banking.loanRecovery`, higher `banking.eclMigrationSensitivity`. |
| Fiscal rules | `fiscal-stabilizer-strong` | Higher `fiscal.govAutoStabMult` and `fiscalConsolidationSpeed55`. |
| Policy rates | `monetary-tight` | Higher `monetary.initialRate`, `neutralRate`, and `taylorAlpha`. |
| External shocks | `external-risk-off` | Activates risk-off shock month and increases FX sensitivity. |

Scenario definitions live in
`src/main/scala/com/boombustgroup/amorfati/config/RobustnessScenarios.scala`.
The runner lives in
`src/main/scala/com/boombustgroup/amorfati/diagnostics/SensitivityRobustnessExport.scala`.

## Metrics

The workflow currently summarizes these Monte Carlo output columns:

| Metric | Interpretation |
| --- | --- |
| `Inflation` | Annualized inflation rate. |
| `Unemployment` | Unemployment share. |
| `MarketWage` | Aggregate market wage. |
| `DebtToGdp` | Government debt to annualized GDP. |
| `DeficitToGdp` | Government deficit to annualized GDP. |
| `CreditToGdpGap` | Macroprudential credit-to-GDP gap. |
| `MinBankCAR` | Minimum bank capital adequacy ratio. |
| `MinBankLCR` | Minimum bank liquidity coverage ratio. |
| `CurrentAccount` | Current-account flow. |
| `ExRate` | PLN/EUR exchange rate. |
| `TotalAdoption` | Automation plus hybrid adoption share. |
| `FirmDeaths` | Monthly firm deaths. |
| `BankFailures` | Failed bank count. |

These metrics are not a replacement for the empirical validation report in
`docs/empirical-validation-report.md`. Robustness asks whether conclusions are
stable across seeds and local parameter changes; empirical validation asks
whether the model matches observed stylized facts.

## Runtime Guidance

Recommended local defaults are intentionally small:

| Use | Command shape | Notes |
| --- | --- | --- |
| Smoke | `--scenario-set smoke --seeds 1 --months 6` | Confirms the workflow compiles, runs, and writes artifacts. |
| Local review | `--scenario-set core --seeds 2 --months 24` | Good default before opening or reviewing a PR. |
| Heavier review | `--scenario-set core --seeds 5 --months 60` | Useful before merging research-sensitive parameter changes. |
| Publication prep | `--scenario-set core --seeds 10+ --months 120+` | Coordinate with the data bridge (#437), `docs/scenario-registry.md`, and `docs/empirical-validation-report.md`. |

Runtime grows roughly with:

```text
scenario_count * seed_count * months
```

The default core run is therefore `9 * 2 * 24 = 432` simulated monthly steps.

## Follow-Ups

- `docs/empirical-validation-report.md` provides the empirical target table
  that these robustness outputs should eventually feed.
- `docs/scenario-registry.md` defines named scenario comparisons that can
  reuse this output shape later.
- #437 should provide source vintages and empirical transformations used to
  interpret robustness against real-world data.
