# Performance Regression Budgets

This document defines the first performance-baseline layer for Amor Fati's
engine diagnostics and hot-path profiling workflows.

The layer is intentionally soft. It produces reviewer-facing regression reports
from existing run manifests; it does not launch extra simulations and it does
not fail workflows on performance drift.

Operational appendix entry point:
[operations.md#operational-appendix-index](operations.md#operational-appendix-index).
Use this page for performance-budget policy after starting from the operations
index.

## Source Artifacts

Performance comparisons reuse `run-manifest.json` files already produced by
`NightlyDiagnosticsProfileRunner`.

The comparison scripts run inside:

```text
.github/workflows/diagnostics-reusable.yml
.github/workflows/hot-path-profiling-reusable.yml
```

Each workflow downloads the newest non-expired artifact produced by a successful
workflow run for the same profile on `main`, extracts the baseline
`run-manifest.json`, and compares it to the current manifest. Failed workflow
runs are ignored even if they uploaded artifacts. Missing permissions, missing
successful artifacts, expired artifacts, or missing manifests produce a
`NO_BASELINE` report rather than a failed job.

No noisy baseline files are committed to the repository.

## Output Artifacts

Each diagnostics or profiling run writes:

```text
<run-root>/performance/performance-regression-report.json
<run-root>/performance/performance-regression-report.md
```

The Markdown report is also appended to the GitHub Actions step summary. The
JSON report contains the current run id, baseline run id, commit metadata,
budget policy, warning count, and per-step metric comparisons.

## Compared Metrics

The initial metrics are deliberately coarse and manifest-derived:

| Metric | Direction | Soft warning threshold |
| --- | --- | --- |
| Step duration (`duration_ms`) | lower is better | current > 1.25 x baseline and +30s |
| Seed-month throughput (`seed_months_per_second`) | higher is better | current < 0.75 x baseline |
| Post-step heap used (`heap_used_bytes_after`) | lower is better | current > 1.40 x baseline and +64 MiB |
| GC time (`gc_time_millis_delta`) | lower is better | current > 1.50 x baseline and +10s |

These thresholds are soft warnings. They are meant to make regressions visible,
not to encode a claim that the engine is too slow.

## Profile Eligibility

`smoke`, `nightly`, and `extended` are eligible for soft budget reports. Each
profile compares only against the newest successful same-profile artifact, so
extended runs do not use smoke or nightly artifacts as baselines.

The extended profile remains observability-only: warnings make long-path runtime
or allocation drift visible, but they do not fail the workflow.

## Branch Validation

Manual workflow dispatch can validate a feature branch before merge:

```bash
gh workflow run diagnostics-smoke.yml --ref <branch>
gh workflow run hot-path-profiling-smoke.yml --ref <branch> -f jfr_settings=profile
```

Scheduled runs always check out `main` and pass `--require-main` to the
diagnostics runner. Manual runs check out the dispatched branch and do not pass
`--require-main`, so branch PRs can verify workflow wiring and inspect the
generated performance reports before merging.

## Promotion Policy

A metric can become a hard gate only after:

- the profile has several comparable successful `main` runs;
- the metric has median-normalized dispersion below 5% across at least 10
  comparable successful `main` runs on GitHub-hosted runners for the same commit,
  runner type, JFR setting, profile, and build parameters;
- the failure message explains the affected step and metric;
- stress-profile failures cannot be mistaken for normal-path performance
  regressions;
- the threshold rationale is documented here.

Measure noise from the generated `performance-regression-report.json` inputs by
collecting the candidate metric across those 10 runs and computing
$\mathrm{standard\_deviation} / \mathrm{median}$. Use the same step id and profile for every sample;
for example, compare only `diagnostics:nightly` duration samples from identical
manual reruns of one `main` commit.

Until then, performance reports are observability evidence. They should guide
review and refactoring, not block correctness work.
