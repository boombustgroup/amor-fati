# Initialization

The init package contains factory objects that build the initial simulation
state from configuration plus an explicit initialization randomness contract.
All factories are stateless objects with pure `create`/`initialize` methods —
no mutable fields.

`WorldInit` also assembles the initial `LedgerFinancialState`. Agent financial
stock DTOs produced during initialization are inputs to that ledger state, not
persisted owner mirrors.

## Files

| File | Object | Role |
|------|--------|------|
| `InitRandomness.scala` | `InitRandomness` | Explicit initialization randomness contract: one root seed split into named streams for firms, households, and initial immigration |
| `WorldInit.scala` | `WorldInit` | Orchestrator — consumes `InitRandomness.Contract`, calls all sub-factories, assembles `World` plus initial `LedgerFinancialState` in `InitResult` |
| `FirmInit.scala` | `FirmInit` | Creates firm array, assigns network topology, applies sector enhancements |
| `BankInit.scala` | `BankInit` | Creates 7-bank sector and computes per-bank financial-stock DTOs from firm/household bank assignments for ledger initialization |
| `ImmigrantInit.scala` | `ImmigrantInit` | Establishes the initial immigrant stock in the labour market |
| `DemographicsInit.scala` | `DemographicsInit` | Seeds the initial retiree cohort that drives ZUS pension expenditure |
| `GvcInit.scala` | `GvcInit` | Seeds Poland's position in global value chains (import content of exports) |
| `InsuranceInit.scala` | `InsuranceInit` | Seeds insurance behavioral/flow state; initial reserves and securities balances are written into `LedgerFinancialState` by `WorldInit` |
| `NbfiInit.scala` | `NbfiInit` | Seeds NBFI behavioral/flow state; initial TFI AUM, securities, cash, and loan balances are written into `LedgerFinancialState` by `WorldInit` |
| `ExpectationsInit.scala` | `ExpectationsInit` | Seeds the forward-looking expectation state used in firm investment and HH saving decisions |
| `HousingInit.scala` | `HousingInit` | Seeds residential housing stock and price level; mortgage stock is aligned from `LedgerFinancialState` by `WorldInit` |
