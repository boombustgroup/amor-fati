# Ledger-Derived SFC Matrix Artifacts

This workflow generates paper-facing Stock-Flow Consistent matrix artifacts from
an executed deterministic simulation step:

- a symbolic Balance Sheet Matrix (BSM);
- a symbolic Transactions Flow Matrix (TFM);
- a mapping from symbolic rows to runtime `AssetType` and `FlowMechanism`
  concepts.

The matrices are intentionally symbolic. They are meant to look like the tables
used in SFC papers and reports.

## Regeneration

Generate LaTeX and Markdown artifacts for seed 1 after 12 executed months:

```bash
sbt "runMain com.boombustgroup.amorfati.diagnostics.SfcMatrixExport --seed 1 --months 12 --out target/sfc-matrices --format tex,md"
```

The shorter sbt input task delegates to the same entrypoint:

```bash
sbt "sfcMatrices --seed 1 --months 12 --out target/sfc-matrices"
```

By default, artifacts are written under `target/sfc-matrices`, so regeneration
does not dirty the repository. The output set is:

- `symbolic-bsm.tex`
- `symbolic-bsm.md`
- `symbolic-tfm.tex`
- `symbolic-tfm.md`
- `matrix-mapping.tex`
- `matrix-mapping.md`

CSV and JSON are deliberately not produced by this exporter. Use the engine or
Monte Carlo CSV output for numeric evidence.

The BSM and TFM LaTeX files are plain `tabular` fragments. The mapping LaTeX
file uses `longtable` so the runtime mapping can break across pages.

## Source Contract

The export still executes the deterministic runtime before writing artifacts:

1. Initialize deterministic state from `WorldInit.initialize`.
2. Execute month steps through `MonthDriver.unfoldSteps`.
3. Build ledger-derived matrix evidence from the selected step.
4. Validate complete BSM rows, exact TFM row sums, stock-flow reconciliation,
   and the underlying `Sfc.validate` status.
5. Render symbolic BSM, symbolic TFM, and the mapping table only if validation
   status is available for the selected run.

The symbolic matrix definitions live in `SfcSymbolicMatrices`. The registry in
`SfcMatrixRegistry` fixes sector metadata, instrument metadata, mechanism
labels, LaTeX symbols, and row coverage policy.

## Sign Conventions

BSM rows use asset-positive and liability-negative signs:

- holder assets are positive;
- issuer or borrower liabilities are negative;
- unsupported or incomplete rows are not silently balanced;
- the net-worth row is a paper-level column-balancing row, not a runtime asset.

TFM rows use payer-negative and receiver-positive signs. Financial-account rows
such as loan origination, repayment, bond issuance, and deposit change follow
the same row-sum-zero convention as the paper matrix.

## Sector Order

The symbolic artifacts use the registry order:

1. Households
2. Firms
3. Banks
4. Government
5. NBP
6. Insurance
7. Funds
8. Foreign

The Funds sector covers public and fund buckets such as ZUS, NFZ, FP, PFRON,
FGSP, JST, PPK, NBFI, and quasi-fiscal vehicles where they are represented in
the runtime ledger topology.

## Coverage Gaps

The symbolic tables are complete paper-level matrices, but the mapping keeps
runtime coverage explicit. Known incomplete rows remain visible as classified
gaps in the ledger-derived validation layer. Examples include:

- household mortgage liabilities without a supported bank-side mortgage stock;
- firm and consumer loan rows where dynamic-population projections can leave
  small holder/issuer gaps across month boundaries;
- bank reserves without a persisted NBP reserve-liability row;
- insurance reserves without holder-resolved household assets;
- equity foreign ownership that remains metric-only;
- bank capital, which is persisted engine state but outside the supported
  ledger-owned stock slice.

These gaps are review evidence. They are not balancing rows.

## Review Checklist

- The export command exits with status 0.
- Only `.tex` and `.md` files are written by the symbolic exporter.
- `symbolic-bsm.*` and `symbolic-tfm.*` contain paper-style symbolic matrices.
- `matrix-mapping.*` links each symbolic row to runtime assets and mechanisms.
- Numeric checks are reviewed against the engine or Monte Carlo CSV output.
