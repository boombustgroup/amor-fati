# Initialization

The init package contains factory objects that build the initial simulation
state from configuration plus an explicit initialization randomness contract.
All factories are stateless objects with pure `create`/`initialize` methods —
no mutable fields.

## Files

| File | Object | Role |
|------|--------|------|
| `InitRandomness.scala` | `InitRandomness` | Explicit initialization randomness contract: one root seed split into named streams for firms, households, and initial immigration |
| `WorldInit.scala` | `WorldInit` | Orchestrator — consumes `InitRandomness.Contract`, calls all sub-factories, assembles `InitResult` |
| `FirmInit.scala` | `FirmInit` | Creates firm array, assigns network topology, applies sector enhancements |
| `BankInit.scala` | `BankInit` | Creates 7-bank sector, distributes deposits/loans/bonds across banks |
| `ImmigrantInit.scala` | `ImmigrantInit` | Establishes the initial immigrant stock in the labour market |
| `DemographicsInit.scala` | `DemographicsInit` | Seeds the initial retiree cohort that drives ZUS pension expenditure |
| `GvcInit.scala` | `GvcInit` | Seeds Poland's position in global value chains (import content of exports) |
| `InsuranceInit.scala` | `InsuranceInit` | Seeds life + non-life reserves that feed back into bond and equity demand |
| `NbfiInit.scala` | `NbfiInit` | Seeds TFI fund AUM as a parallel credit channel alongside the banking sector |
| `ExpectationsInit.scala` | `ExpectationsInit` | Seeds the forward-looking expectation state used in firm investment and HH saving decisions |
| `HousingInit.scala` | `HousingInit` | Seeds residential housing stock, price level, and mortgage debt |
