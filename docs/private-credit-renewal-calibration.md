# Private Credit Renewal Calibration

Issue #610 re-checks the 60-month Poland baseline after the #523 final run
showed private-credit compression that was no longer caused by bank-resolution
fallbacks.

## Accepted Change

NBFI credit origination is stock-renewal based:

$$
\begin{aligned}
origination &= openingNbfiLoanStock \cdot \mathrm{nbfi.creditBaseRate} \\
&\quad \cdot (1 + countercyclical \cdot bankTightness)
\end{aligned}
$$

The baseline `nbfi.creditBaseRate` is `0.031` per month. This is calibrated
against the 36-month scheduled repayment rate and baseline defaults, so NBFI
loan stock no longer mechanically runs off because origination was tied to
domestic consumption rather than the opening book.

## Rejected Levers

Two wider renewal levers were tested and rejected for this PR:

- Increasing `housing.originationRate` to `0.0045` improved the mortgage slice
  but caused an all-failed banking-sector failure in the 10-seed 60-month run.
- Partial firm-credit approval after hard full-size rejection also caused an
  all-failed banking-sector failure in the 10-seed 60-month run.

Those channels need a follow-up that treats bank-capital consequences
explicitly instead of calibrating private credit by adding bank exposure.

## Validation Run

Command:

```bash
nix develop -c java -jar target/scala-3.8.2/amor-fati.jar 10 issue610-nbfi-final --duration 60 --run-id 20260525-610-final-nbfi --firm-snapshots none --household-snapshots none
```

JAR hash: `fe8e8e0d472a2488d44de95379d8b1db3de38c4b`.

The run completed all 10 seeds and wrote:

```text
mc/issue610-nbfi-final_20260525-610-final-nbfi_60m_seed001.tsv
...
mc/issue610-nbfi-final_20260525-610-final-nbfi_60m_seed010.tsv
```

Terminal `TotalCreditToGdp` improved from the #523 final baseline mean of
`18.17%` to `21.50%` (`20.81%` to `22.17%` seed range). The terminal split was:

| Metric | Mean | Seed range |
| --- | ---: | ---: |
| `BankFirmLoansToGdp` | `6.13%` | `5.81%` to `6.42%` |
| `ConsumerLoansToGdp` | `2.12%` | `2.00%` to `2.24%` |
| `MortgageToGdp` | `8.76%` | `8.20%` to `9.10%` |
| `NbfiLoansToGdp` | `4.50%` | `4.21%` to `4.67%` |

The NBFI terminal flow decomposition was:

| Metric | Mean |
| --- | ---: |
| `NbfiOriginationToStock` | `3.10%` |
| `NbfiRepaymentToStock` | `2.77%` |
| `NbfiDefaultsToStock` | `0.21%` |
| `NbfiNetStockFlow` | `0.29bn PLN/month` |

Bank-resolution sanity stayed clean:

| Metric | Terminal result |
| --- | ---: |
| `BankResolution_AllFailedFallback` | `0` all seeds |
| `BankFailure_AllFailedFallback` | `0` all seeds |
| `BankResolution_InvalidActiveBankInvariant` | `0` all seeds |
| `BankFailure_InvariantViolation` | `0` all seeds |

The remaining compression is therefore not an NBFI renewal-base artifact. The
remaining structural channels are firm-credit renewal, household credit/default
pressure, mortgage renewal under bank-capital consequences, and GDP denominator
growth.
