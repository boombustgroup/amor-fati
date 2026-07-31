# External-Sector Baseline Calibration

Issue: #617

This note records the baseline calibration that followed the current-account
closure audit in #522 and the diagnostic export added in #616. The accounting
identity was already exact: `CurrentAccountClosureResidual` stayed at zero. The
remaining problem was behavioral. In a 60-month baseline run, GVC exports drifted
down relative to domestic output capacity, which pushed the current-account
deficit mechanically wider even though the closure terms balanced.

## Decision

Keep `openEcon.exportBase` unchanged. Add a domestic export-capacity term inside
`GvcTrade`:

$$
\begin{aligned}
sectorExportDemand &=
foreignDemand
\cdot realExchangeRateEffect
\cdot automationEffect \\
&\quad \cdot
\left(\frac{realSectorOutput}{openingRealSectorOutput}\right)^{\mathrm{gvc.exportCapacityElasticity}}
\cdot disruptionEffect
\end{aligned}
$$

The opening real sector output is anchored on the first observed sector-output
vector. This preserves the empirical opening export base while allowing realized
exports to move with domestic supply capacity over a multi-year baseline.

The calibrated value is:

$$
\mathrm{gvc.exportCapacityElasticity} = 0.35
$$

A trial value of `0.55` was rejected. It improved the external balance more
aggressively but produced an all-failed banking-sector path in the Nix/JAR
5-seed 60-month validation run, so it was not a stable baseline setting.

## Validation

Fresh JAR:

```sh
nix develop -c sbt assembly
```

Baseline command:

```sh
nix develop -c java -jar target/scala-3.8.2/amor-fati.jar 5 issue617-external-baseline --duration 60 --run-id 20260525-617-external-baseline-after035 --firm-snapshots none --household-snapshots none
```

The run completed all five seeds and wrote:

```text
mc/issue617-external-baseline_20260525-617-external-baseline-after035_60m_seed001.tsv
mc/issue617-external-baseline_20260525-617-external-baseline-after035_60m_seed002.tsv
mc/issue617-external-baseline_20260525-617-external-baseline-after035_60m_seed003.tsv
mc/issue617-external-baseline_20260525-617-external-baseline-after035_60m_seed004.tsv
mc/issue617-external-baseline_20260525-617-external-baseline-after035_60m_seed005.tsv
```

Annual ratios are computed as $\frac{\sum \mathrm{flow}}{\sum \mathrm{MonthlyGdpProxy}}$ across the five
seeds in each 12-month window. M60 rows are seed means.

## Results

Before: post-#616 baseline,
`run-id=20260525-external-baseline-before`.

After: `gvc.exportCapacityElasticity=0.35`,
`run-id=20260525-617-external-baseline-after035`.

| Window | CA/GDP before | CA/GDP after | Trade/GDP before | Trade/GDP after | Exports/GDP before | Exports/GDP after | Imports/GDP before | Imports/GDP after |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Y1 | -6.50% | -6.14% | -5.05% | -4.67% | 47.81% | 48.17% | 52.86% | 52.84% |
| Y2 | -4.71% | -3.84% | -3.95% | -2.97% | 44.39% | 45.40% | 48.33% | 48.37% |
| Y3 | -6.51% | -4.85% | -5.42% | -3.68% | 41.79% | 43.54% | 47.21% | 47.22% |
| Y4 | -8.58% | -6.38% | -6.49% | -4.30% | 39.99% | 42.11% | 46.49% | 46.41% |
| Y5 | -10.10% | -7.78% | -6.90% | -4.59% | 39.08% | 41.13% | 45.98% | 45.72% |

| M60 metric | Before | After |
| --- | ---: | ---: |
| CurrentAccountToGdp | -11.42% | -9.15% |
| TradeBalanceToGdp | -7.91% | -5.55% |
| ExportsToGdp | 36.63% | 38.73% |
| ImportsToGdp | 44.54% | 44.29% |
| CurrentAccountPrimaryIncomeToGdp | -0.95% | -0.74% |
| CurrentAccountSecondaryIncomeToGdp | 0.89% | 0.87% |
| ImportedIntermToImports | 73.52% | 73.29% |
| ExRate | 4.4431 | 4.3482 |
| BankResolution_AllFailedFallback | 0 | 0 |
| BankFailure_AllFailedFallback | 0 | 0 |
| max abs CurrentAccountClosureResidual | 0 | 0 |

## Interpretation

The calibration reduces the mechanical widening of the current-account deficit
without forcing a surplus or masking the structural external deficit. The
improvement comes from export realization tracking domestic output capacity; the
import side remains largely unchanged, and the current-account accounting
closure remains exact.
