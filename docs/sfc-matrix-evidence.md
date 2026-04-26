# Ledger-Derived SFC Matrix Artifacts

This workflow generates paper-facing Stock-Flow Consistent matrix artifacts from
an executed deterministic simulation step:

- a symbolic Balance Sheet Matrix (BSM);
- a symbolic Transactions Flow Matrix (TFM);
- a Stock-Flow Reconciliation and Revaluation Evidence table;
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
- `stock-flow-reconciliation.tex`
- `stock-flow-reconciliation.md`
- `matrix-mapping.tex`
- `matrix-mapping.md`

The Balance Sheet Matrix and Transactions Flow Matrix LaTeX files are plain
`tabular` fragments. The reconciliation and mapping LaTeX files use `longtable`
so runtime evidence can break across pages.

## Source Contract

The export still executes the deterministic runtime before writing artifacts:

1. Initialize deterministic state from `WorldInit.initialize`.
2. Execute month steps through `MonthDriver.unfoldSteps`.
3. Build ledger-derived validation inputs from the selected step.
4. Validate complete Balance Sheet Matrix rows, exact Transactions Flow Matrix
   row sums, stock-flow reconciliation, and the underlying `Sfc.validate`
   status.
5. Render symbolic Balance Sheet Matrix, symbolic Transactions Flow Matrix,
   Stock-Flow Reconciliation and Revaluation Evidence, and the mapping table
   for the selected run.

The symbolic matrix definitions live in `SfcSymbolicMatrices`. The registry in
`SfcMatrixRegistry` fixes sector metadata, instrument metadata, mechanism
labels, LaTeX symbols, and row coverage policy.

## Sign Conventions

Balance Sheet Matrix rows use asset-positive and liability-negative signs:

- holder assets are positive;
- issuer or borrower liabilities are negative;
- unsupported or incomplete rows are not silently balanced;
- the net-worth row is a paper-level column-balancing row, not a runtime asset.

Transactions Flow Matrix rows use payer-negative and receiver-positive signs.
Financial-account rows such as loan origination, repayment, bond issuance, and
deposit change follow the same row-sum-zero convention as the paper matrix.

## Stock-Flow Reconciliation And Revaluation

`stock-flow-reconciliation.*` is not a residual balancing row. It renders the
15 exact SFC identities used by `Sfc.validate`:

- actual values come from observed opening and closing `Sfc.StockState` values
  or level-clearing checks;
- expected values come from `Sfc.SemanticFlows`, which is assembled from
  executed runtime mechanisms where a mechanism exists;
- the row residual is `actual - expected` and must be zero for a passing row.

The artifact deliberately avoids validating `otherChange = stockDelta -
transactionDelta`. That diagnostic residual remains useful for coverage review,
but it is not used as proof that stock-flow reconciliation holds.

Runtime channel coverage:

- FX and NFA valuation: `OpenEconEconomics.valuationEffect`, combined with
  current-account channels such as trade, tourism, primary income, EU funds,
  remittances, and capital flight.
- Defaults and write-offs: consumer credit, firm NPL, mortgage, corporate bond,
  and NBFI default mechanisms, plus bank loss-recognition mechanisms.
- Bond valuation and loss recognition: `BankUnrealizedLoss`, plus
  `htmRealizedLoss` and `eclProvisionChange` from `BankingEconomics`.
- Bond clearing: government and quasi-fiscal holder/issuer level checks over
  supported bank, NBP, insurance, fund, foreign, and issuer stocks.
- Equity valuation: equity returns enter insurance and NBFI investment-income
  channels. A holder-resolved exact equity-stock revaluation row still requires
  a first-class runtime equity revaluation mechanism and complete persisted
  equity holder stocks.
- Other changes in volume: bank capital destruction, bail-in, amortization,
  repayment, and quasi-fiscal lending channels are explicit components of the
  exact identities where they affect supported stocks.

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

These gaps are review diagnostics. They are not balancing rows.

## Review Checklist

- The export command exits with status 0.
- Only `.tex` and `.md` files are written by the symbolic exporter.
- `symbolic-bsm.*` contains the paper-style symbolic Balance Sheet Matrix.
- `symbolic-tfm.*` contains the paper-style symbolic Transactions Flow Matrix.
- `stock-flow-reconciliation.*` contains exact SFC identity evidence with
  expected, actual, residual, status, and runtime channels.
- `matrix-mapping.*` links each symbolic row to runtime assets and mechanisms.
- `matrix-mapping.tex` is included with `\usepackage{longtable}`.
- `stock-flow-reconciliation.tex` is included with `\usepackage{longtable}`.
