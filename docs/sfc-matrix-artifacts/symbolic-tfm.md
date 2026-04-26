<!-- schema=sfc-matrix-v1 seed=1 month=12 commit=50a565f7 sfc=pass matrix=pass output=symbolic -->
# Symbolic Transactions Flow Matrix
| Flow \\ Sector | Households | Firms | Banks | Government | NBP | Insurance | Funds | Foreign | Sum |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Consumption | -C_h | +C_h |  |  |  |  |  |  | 0 |
| Wages and household income | +W | -W |  |  |  |  |  |  | 0 |
| Taxes | -T_h | -T_f | -T_b | +T |  |  |  |  | 0 |
| Social contributions | -SC_h | -SC_f |  |  |  |  | +SC |  | 0 |
| Transfers and benefits | +TR_h |  |  | -TR_g |  |  | -TR_f |  | 0 |
| Government spending |  | +G |  | -G |  |  |  |  | 0 |
| Insurance premiums and claims | -P+CL |  |  |  |  | +P-CL |  |  | 0 |
| Loan interest | -rL_h | -rL_f | +rL |  |  |  |  |  | 0 |
| Deposit and reserve interest | +rD_h |  | -rD+rR |  | -rR |  |  |  | 0 |
| Bond coupons |  | -iB_c | +iB_b | -iB_g |  | +iB_i | +iB_f | +iB_{row} | 0 |
| Dividends | +Div_h | -Div |  | +Div_g |  | +Div_i | +Div_f | +Div_{row} | 0 |
| External trade and income | +REM | +X-M |  | +EU |  |  |  | -NX-REM-EU | 0 |
| Loan origination | +dL_h | +dL_f | -dL |  |  |  |  |  | 0 |
| Loan repayment and defaults | -repL_h | -repL_f | +repL |  |  |  |  |  | 0 |
| Bond issuance and purchases |  | +dB_c | -dB_b | +dB_g | -dB_{nbp} | -dB_i | -dB_f | -dB_{row} | 0 |
| Deposit change | -dD_h | -dD_f | +dD |  |  |  | -dD_{fnd} |  | 0 |
