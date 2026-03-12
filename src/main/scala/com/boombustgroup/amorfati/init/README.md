# Initialization

The init package contains factory objects that build the initial simulation
state from a seed and configuration. All factories are stateless objects with
pure `create`/`initialize` methods — no mutable fields.

## Files

| File | Object | Role |
|------|--------|------|
| `WorldInit.scala` | `WorldInit` | Orchestrator — calls all sub-factories, assembles `InitResult` |
| `FirmInit.scala` | `FirmInit` | Creates firm array, assigns network topology, applies sector enhancements |
| `BankInit.scala` | `BankInit` | Creates 7-bank sector, distributes deposits/loans/bonds across banks |
| `ImmigrantInit.scala` | `ImmigrantInit` | Establishes the initial immigrant stock in the labour market |
| `DemographicsInit.scala` | `DemographicsInit` | Seeds the initial retiree cohort that drives ZUS pension expenditure |
| `EquityInit.scala` | `EquityInit` | Seeds GPW index level and household equity wealth from the savings distribution |
| `GvcInit.scala` | `GvcInit` | Seeds Poland's position in global value chains (import content of exports) |
| `InsuranceInit.scala` | `InsuranceInit` | Seeds life + non-life reserves that feed back into bond and equity demand |
| `NbfiInit.scala` | `NbfiInit` | Seeds TFI fund AUM as a parallel credit channel alongside the banking sector |
| `ExpectationsInit.scala` | `ExpectationsInit` | Seeds the forward-looking expectation state used in firm investment and HH saving decisions |
| `HousingInit.scala` | `HousingInit` | Seeds residential housing stock, price level, and mortgage debt |

`WorldInit` is the single entry point called from `McRunner.runSingle`.
