<!-- schema=sfc-matrix-v1 seed=1 month=12 commit=50a565f7 sfc=pass matrix=pass output=symbolic -->
# Symbolic Balance Sheet Matrix
| Instrument \\ Sector | Households | Firms | Banks | Government | NBP | Insurance | Funds | Foreign | Sum |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Cash and public balances | +H_h | +H_f | +H_b |  | -H |  | +H_{pub} |  | 0 |
| Demand deposits | +D_h | +D_f | -D |  |  |  | +D_{fnd} |  | 0 |
| Term deposits | +TD_h |  | -TD |  |  |  |  |  | 0 |
| Loans | -L_h | -L_f | +L |  |  |  |  |  | 0 |
| Bank reserves |  |  | +R |  | -R |  |  |  | 0 |
| Government bonds |  |  | +B_b | -B_g | +B_{nbp} | +B_{ins} | +B_{fnd} | +B_{row} | 0 |
| Quasi-fiscal bonds |  |  | +Q_b |  | +Q_{nbp} |  | -Q |  | 0 |
| Corporate bonds |  | -B_c | +B_{cb} |  |  | +B_{ci} | +B_{cf} |  | 0 |
| Equity | +E_h | -E |  |  |  | +E_i | +E_f | +E_{row} | 0 |
| Insurance reserves | +IR_h |  |  |  |  | -IR |  |  | 0 |
| Fund units | +U_h |  |  |  |  |  | -U |  | 0 |
| Foreign assets |  |  |  |  | +FA |  |  | -FA | 0 |
| Net worth | -NW_h | -NW_f | -NW_b | -NW_g | -NW_{nbp} | -NW_{ins} | -NW_{fnd} | -NW_{row} | 0 |
