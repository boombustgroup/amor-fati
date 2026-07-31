# Model Equations To SFC Map

This hand-maintained document maps Amor Fati's publication-facing equation
families to the ledger-derived SFC matrix rows, runtime stock families,
flow-channel semantics, and exact reconciliation identities.

It is not generated, does not define new model behavior, and does not duplicate
the generated matrix tables.

## Source Contract

The authoritative SFC artifacts are generated automatically:

- [SFC matrix evidence](sfc-matrix-evidence.md);
- [symbolic BSM](sfc-matrix-artifacts/symbolic-bsm.md);
- [symbolic TFM](sfc-matrix-artifacts/symbolic-tfm.md);
- [symbolic-row to runtime mapping](sfc-matrix-artifacts/matrix-mapping.md);
- [flow-channel semantics](sfc-matrix-artifacts/flow-mechanism-semantics.md);
- [stock-flow reconciliation](sfc-matrix-artifacts/stock-flow-reconciliation.md).

This map is intentionally hand-maintained because it explains how model equation
families should be read against those generated artifacts. The generated
artifacts remain the source of truth for exact row names, runtime mechanisms,
asset families, identity status, and per-run evidence.

## Maintenance Contract

Changes to generated SFC row names, identity names, runtime asset coverage, or
flow-channel coverage policy must update this document in the same PR. The
generated-output guard proves that generated artifacts are fresh; this document
is the companion human-maintained index that keeps equation-family prose aligned
with those artifacts.

Future linting can parse the generated matrix artifacts and fail if this map
references a removed row or identity. Until that exists, reviewers should treat
this document as part of the SFC artifact review surface.

## Status Vocabulary

| Status | Meaning |
| --- | --- |
| `Exact identity` | The family is covered by one or more exact rows in `stock-flow-reconciliation.md`. |
| `Matrix row` | The family appears as a BSM or TFM row, but exactness may be enforced elsewhere or through ledger conservation. |
| `Execution delta only` | Runtime mechanisms explain executed deltas but do not persist as transferable stock ownership. |
| `Diagnostic only` | The value is a model diagnostic or policy/risk surface, not a monetary flow or holder-owned stock. |
| `Unsupported persisted stock` | Runtime state persists a stock that is SFC-audited but not supported as holder-resolved ledger ownership. |
| `Known limitation` | The artifact deliberately exposes a missing holder split, residual, or unsupported economic ownership channel. |

## Equation Family Map

| Equation family | Equation sources | State and stock variables | SFC rows and identities | Runtime evidence | Status and notes |
| --- | --- | --- | --- | --- | --- |
| Monthly transition and ledger execution | [monthly-transition-function.md](monthly-transition-function.md), [model-specification.md](model-specification.md#monthly-transition-function) | $X_{t}$, $X_{\tau}$, `LedgerFinancialState`, executed flow batches | BSM all stock rows, TFM all transaction rows, `Flow of funds` identity | `RuntimeFlowExecutor`, `Sfc.validate`, generated BSM/TFM, `Flow of funds` reconciliation row | `Exact identity`: runtime ledger conservation and semantic SFC validation own the accounting boundary. |
| Household income, consumption, deposits, and transfers | [household-equations.md](household-equations.md), [behavioral-equations-and-decision-rules.md#household-rules](behavioral-equations-and-decision-rules.md#household-rules) | Household deposits, term deposits, income, consumption, taxes, transfers, insurance reserves, fund units | TFM rows: `Wages and household income`, `Consumption`, `Taxes`, `Transfers and benefits`, `Insurance premiums and claims`, `Deposit change`; BSM rows: `Demand deposits`, `Term deposits`, `Insurance reserves`, `Fund units`; identity: `Bank deposits` | `HhTotalIncome`, `HhConsumption`, `HhPit`, `HhDepositInterest`, insurance and fund mechanisms in flow-channel semantics | `Exact identity` for bank deposits. Insurance reserves and fund units are `Matrix row` surfaces here, not separate stock-flow reconciliation identities. Household utility, distress, social-neighbor, and retraining signals are behavioral/diagnostic unless they realize a mapped flow. |
| Household consumer credit, liquidity bridge, and distress losses | [consumer credit](household-equations.md#consumer-credit), [liquidity shortfall settlement](household-equations.md#liquidity-shortfall-settlement), [banking-and-financial-sector-equations.md](banking-and-financial-sector-equations.md) | Consumer loan stock, consumer defaults, liquidity shortfall financing, consumer NPL loss | TFM rows: `Loan origination`, `Loan repayment and defaults`; BSM row: `Loans`; identities: `Consumer credit`, `Bank capital` | `HhCcOrigination`, `HhLiquidityShortfallFinancing`, `HhCcDebtService`, `HhCcDefault`, `BankCcNplLoss` | `Exact identity` for consumer loan stock. Consumer credit loss recognition is separate from ECL Stage 3 migration in current runtime. |
| Mortgages and housing finance | [household-equations.md](household-equations.md), [banking-and-financial-sector-equations.md](banking-and-financial-sector-equations.md) | Mortgage loan stock, origination, repayment, default, mortgage interest, house-price diagnostics | TFM rows: `Loan origination`, `Loan repayment and defaults`, `Loan interest`; BSM row: `Loans`; identities: `Mortgage stock`, `Bank capital` | `MortgageOrigination`, `MortgageRepayment`, `MortgageDefault`, `MortgageInterest`, `BankMortgageNplLoss` | `Exact identity` for mortgage stock and bank loss recognition. House-price index and affordability ratios are `Diagnostic only`. |
| Labor, payroll funds, and demographics | [behavioral-equations-and-decision-rules.md#labor-demographics-and-social-funds](behavioral-equations-and-decision-rules.md#labor-demographics-and-social-funds), [institutional-sector-equations.md](institutional-sector-equations.md) | Employment, wages, working-age population, ZUS/NFZ/PPK/FP/PFRON/FGSP balances | TFM rows: `Wages and household income`, `Social contributions`, `Transfers and benefits`, `Government spending`; BSM row: `Cash and public balances` | ZUS, NFZ, PPK, FP, PFRON, FGSP mechanisms in flow-channel semantics | `Matrix row`: payroll and benefit flows are matrix-routed. Demographic state changes are not monetary flows. |
| Firm production, pricing, inventory, investment, and technology | [firm-equations.md](firm-equations.md), [behavioral-equations-and-decision-rules.md#firm-rules](behavioral-equations-and-decision-rules.md#firm-rules) | Firm cash, production, capacity, inventory, technology state, investment, capex, equity issuance | TFM rows: `Consumption`, `Government spending`, `External trade and income`, selected `Deposit change` and firm operating-flow rows through flow-channel semantics | Firm operating mechanisms such as `FirmCapex`, `FirmGrossInvestment`, `FirmEquityIssuance`, `InvestmentDepositSettlement` | `Execution delta only` for several firm operating channels. Physical production, capacity, inventory, and technology are real-side state variables and diagnostics unless they emit mapped cash, tax, wage, investment, loan, bond, equity, or external flows. |
| Firm taxes, interest, bank credit, default, and NPL | [financing and credit approval](firm-equations.md#financing-and-credit-approval), [default, NPL, and exit](firm-equations.md#default-npl-and-exit), [banking-and-financial-sector-equations.md](banking-and-financial-sector-equations.md) | Firm loan stock, firm NPL stock, firm interest, CIT, bank firm-loan interest, bank NPL loss | TFM rows: `Taxes`, `Loan interest`, `Loan origination`, `Loan repayment and defaults`; BSM row: `Loans`; identity: `Bank capital`; loan rows through semantic evidence | `FirmCit`, `FirmInterestPaid`, `FirmNewLoan`, `FirmLoanRepayment`, `FirmNplDefault`, `BankFirmInterest`, `BankNplLoss` | `Exact identity` for bank capital; firm loan origination, repayment, and default are matrix-routed loan-flow channels. Firm NPL diagnostics are decision and loss surfaces, not separate holder-owned stocks. |
| Corporate bonds | [firm-equations.md](firm-equations.md), [banking-and-financial-sector-equations.md](banking-and-financial-sector-equations.md), [institutional-sector-equations.md](institutional-sector-equations.md) | Corporate bond outstanding, bank/insurance/fund/other holder stocks, yield, coupon, amortization, default | TFM rows: `Bond coupons`, `Bond issuance and purchases`; BSM row: `Corporate bonds`; identities: `Corporate bond stock` and `Bank capital` for bank-held losses | `CorpBondCoupon`, `CorpBondDefault`, `CorpBondIssuance`, `CorpBondAmortization`, `BankCorpBondCoupon`, `BankCorpBondLoss` | `Exact identity` for issuer stock; holder-level loss recognition is split across bank and non-bank channels. |
| Banking rates, approval gates, regulatory ratios, interbank, and reserves | [banking-and-financial-sector-equations.md](banking-and-financial-sector-equations.md), [behavioral-equations-and-decision-rules.md#banking-rules](behavioral-equations-and-decision-rules.md#banking-rules) | Bank deposits, reserves, interbank position, CAR, LCR, NSFR, NPL ratio, approval probability, policy rates | TFM rows: `Deposit and reserve interest`, `Loan interest`; BSM rows: `Demand deposits`, `Term deposits`, `Bank reserves`, `Loans`; identities: `Bank deposits`, `Interbank netting`, `Bank capital` | `BankReserveInterest`, `BankStandingFacility`, `BankInterbankInterest`, `HhDepositInterest`, bank credit approval diagnostics | `Exact identity` for deposits, interbank netting, and capital P&L. CAR/LCR/NSFR, approval probabilities, and pricing spreads are `Diagnostic only`/decision surfaces until they realize flows. |
| Bank capital, ECL, failures, bail-in, and resolution | [banking-and-financial-sector-equations.md](banking-and-financial-sector-equations.md), [engine-invariants-and-semantics.md](engine-invariants-and-semantics.md), [bank-balance-sheet-benchmark.md](bank-balance-sheet-benchmark.md) | `BankState.capital`, ECL stages, retained income, realized credit losses, bail-in haircut, failure/resolution counters | Identity: `Bank capital`; TFM row: `Deposit change` for bail-in/deposit-side effects; BSM rows: bank deposits and supported transferred assets | Bank-capital channels in `stock-flow-reconciliation.md`, `BankBailIn`, `BankFailure_*`, `BankResolution_*` outputs | `Unsupported persisted stock`: bank capital is SFC-audited regulatory state, not holder-resolved bank equity. Bail-in is depositor-side deposit reduction, not equity transfer. |
| Government, public funds, debt, and spending | [institutional-sector-equations.md](institutional-sector-equations.md), [behavioral-equations-and-decision-rules.md#government-budget-and-debt](behavioral-equations-and-decision-rules.md#government-budget-and-debt) | Government cash, public-fund balances, taxes, spending, transfers, government bonds, debt-service flows | TFM rows: `Taxes`, `Social contributions`, `Transfers and benefits`, `Government spending`, `Bond coupons`, `Bond issuance and purchases`; BSM rows: `Cash and public balances`, `Government bonds`; identity: `Government bond clearing` | Government, social-fund, JST, PPK, and government-bond mechanisms in generated flow-channel semantics | `Exact identity` for supported government-bond clearing. Public cash, tax, spending, and fund flows are matrix-routed channels; some legacy public-sector identities remain outside the rendered stock-flow reconciliation artifact. Fiscal-rule diagnostics and deficit/debt ratios are `Diagnostic only`. |
| NBP, monetary policy, QE, FX, and NFA | [institutional-sector-equations.md](institutional-sector-equations.md), [behavioral-equations-and-decision-rules.md#nbp-policy-bond-yield-qe-fx](behavioral-equations-and-decision-rules.md#nbp-policy-bond-yield-qe-fx) | NBP reserves liability, FX reserves/foreign assets, reference rate, QE holdings, monetary aggregates | TFM rows: `Deposit and reserve interest`, `Bond issuance and purchases`, `External trade and income`; BSM rows: `Bank reserves`, `Government bonds`, `Foreign assets`; identities: `Net foreign assets`, `Government bond clearing` | `BankReserveInterest`, `BankStandingFacility`, `NbpQeGovBondPurchase`, `NbpFxSettlement`, external current-account mechanisms | `Exact identity` for NFA and government-bond clearing. Bank reserves are a BSM row and reserve/facility flows are TFM/SFC channels, not a separate rendered stock-flow reconciliation identity. Policy reaction variables and monetary aggregates are mostly decision/diagnostic surfaces. |
| External sector, trade, FDI, remittances, tourism, and capital flows | [institutional-sector-equations.md](institutional-sector-equations.md), [behavioral-equations-and-decision-rules.md#external-sector](behavioral-equations-and-decision-rules.md#external-sector) | Foreign assets/NFA, exports, imports, remittances, tourism, primary income, EU funds, capital flight, FDI state | TFM row: `External trade and income` for current-account channels; BSM row: `Foreign assets`; identity: `Net foreign assets`; capital-account channels through flow semantics | `TradeExports`, `TradeImports`, `TourismExport`, `TourismImport`, `Fdi`, `PortfolioFlow`, `PrimaryIncome`, `EuFunds`, `DiasporaInflow`, `HhRemittance`, `CapitalFlight`, `CarryTradeFlow`, `FirmProfitShifting`, `FirmFdiRepatriation` | `Exact identity` for NFA. FDI ownership state and profit-shifting/repatriation are routed where mechanisms exist, with remaining holder-detail limits kept explicit in flow semantics. |
| Insurance, TFI/NBFI, PPK, and quasi-fiscal vehicles | [institutional-sector-equations.md](institutional-sector-equations.md), [banking-and-financial-sector-equations.md#insurance-nbfi-tfi-and-quasi-fiscal-interfaces](banking-and-financial-sector-equations.md#insurance-nbfi-tfi-and-quasi-fiscal-interfaces) | Insurance reserves, premiums/claims, TFI AUM/fund units, NBFI loan stock, PPK holdings, quasi-fiscal bonds and loans | TFM rows: `Insurance premiums and claims`, `Bond coupons`, `Bond issuance and purchases`, `Deposit change`, `Loan origination`, `Loan repayment and defaults`; BSM rows: `Insurance reserves`, `Fund units`, `Quasi-fiscal bonds`; identities: `NBFI credit`, `Quasi-fiscal bond stock`, `Quasi-fiscal bond clearing`, `Quasi-fiscal bank bond holdings`, `Quasi-fiscal NBP bond holdings`, `Quasi-fiscal credit` | Insurance, NBFI, PPK, TFI, and quasi-fiscal mechanisms in flow-channel semantics | `Exact identity` for NBFI and quasi-fiscal stock families. Insurance reserves, fund units, and PPK/fund holder legs are matrix-routed surfaces unless listed as exact identities in `stock-flow-reconciliation.md`. Non-bank institutions remain aggregate agents; holder-resolution detail is a known modeling limitation. |
| Demand, GDP, prices, expectations, macroprudential, and calibration diagnostics | [behavioral-equations-and-decision-rules.md#demand-prices-gdp-and-equity](behavioral-equations-and-decision-rules.md#demand-prices-gdp-and-equity), [empirical-validation-report.md](empirical-validation-report.md), [calibration-register.md](calibration-register.md) | GDP proxy, price level, inflation, expectations, credit gap, countercyclical buffer, empirical targets | No direct BSM/TFM rows unless downstream realized flows are emitted by another family | Monte Carlo columns, diagnostics, calibration register, empirical validation snapshot | `Diagnostic only`: macro observables and policy/risk state explain behavior and validation targets but are not ledger-owned stocks or standalone SFC flows. |

## Unsupported Or Diagnostic-Only Rows

The generated artifacts deliberately expose unsupported and diagnostic-only
surfaces instead of silently balancing them:

- `Net worth` is a paper-level BSM column-balancing row. It is not a runtime
  asset.
- `Bank capital` is a persisted operational stock and exact SFC diagnostic, but
  it is not supported as transferable holder-owned bank equity.
- CAR, LCR, NSFR, NPL ratios, approval probabilities, unemployment, GDP proxy,
  inflation, expectations, and scenario/validation metrics are decision or
  observation surfaces. They become SFC-relevant only through realized mapped
  flows or stock changes.
- Physical production, capacity, inventory, technology adoption, demographics,
  and migration are real-side state changes unless they trigger mapped monetary
  flows.
- Several firm operating channels are execution-delta-only; their economic
  meaning is documented in
  [flow-mechanism-semantics.md](sfc-matrix-artifacts/flow-mechanism-semantics.md)
  rather than converted into holder-resolved persisted stocks.

## Reviewer Use

Use this document as the bridge from equations to accounting evidence:

1. Start with the relevant sector equation document.
2. Find the model family above.
3. Follow the listed BSM/TFM rows and exact identities to generated artifacts.
4. Use the generated matrix mapping and flow-channel semantics to inspect
   runtime assets, mechanisms, ids, survivability class, and test/diagnostic
   coverage.
5. Treat any `Diagnostic only`, `Execution delta only`,
   `Unsupported persisted stock`, or `Known limitation` note as part of the
   model contract, not as an implicit missing flow.
