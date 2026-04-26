<!-- schema=sfc-matrix-v1 seed=1 month=12 commit=50a565f7 sfc=pass matrix=pass output=symbolic-mapping -->
# Symbolic Matrix Mapping
| Matrix | Row | Symbols | Runtime assets | Runtime mechanisms | Note |
| --- | --- | --- | --- | --- | --- |
| symbolic-bsm | Cash and public balances | +H_h, +H_f, +H_b, -H, +H_{pub}, 0 | Cash and public fund balances (Cash) |  | Cash covers persisted firm, selected public-fund, and NBFI cash plus the paper-level currency shell. |
| symbolic-bsm | Demand deposits | +D_h, +D_f, -D, +D_{fnd}, 0 | Demand deposits (DemandDeposit) |  |  |
| symbolic-bsm | Term deposits | +TD_h, -TD, 0 | Term deposits (TermDeposit) |  |  |
| symbolic-bsm | Loans | -L_h, -L_f, +L, 0 | Firm loans (FirmLoan)<br>Consumer loans (ConsumerLoan)<br>Mortgage loans (MortgageLoan)<br>NBFI loans (NbfiLoan) |  | Loan rows aggregate firm, consumer, mortgage, and NBFI credit instruments. |
| symbolic-bsm | Bank reserves | +R, -R, 0 | Bank reserves (Reserve) |  | The NBP reserve-liability side is a runtime settlement shell in the current engine. |
| symbolic-bsm | Government bonds | +B_b, -B_g, +B_{nbp}, +B_{ins}, +B_{fnd}, +B_{row}, 0 | Government bonds (GovBondHTM)<br>Government bonds AFS (GovBondAFS) |  |  |
| symbolic-bsm | Quasi-fiscal bonds | +Q_b, +Q_{nbp}, -Q, 0 | Quasi-fiscal bonds (QuasiFiscalBond) |  |  |
| symbolic-bsm | Corporate bonds | -B_c, +B_{cb}, +B_{ci}, +B_{cf}, 0 | Corporate bonds (CorpBond) |  |  |
| symbolic-bsm | Equity | +E_h, -E, +E_i, +E_f, +E_{row}, 0 | Equity (Equity) |  | Foreign equity ownership is currently metric-level evidence rather than a holder-resolved persisted stock. |
| symbolic-bsm | Insurance reserves | +IR_h, -IR, 0 | Life insurance reserves (LifeReserve)<br>Non-life insurance reserves (NonLifeReserve) |  |  |
| symbolic-bsm | Fund units | +U_h, -U, 0 | TFI units (TfiUnit) |  |  |
| symbolic-bsm | Foreign assets | +FA, -FA, 0 | Foreign assets (ForeignAsset) |  |  |
| symbolic-bsm | Net worth | -NW_h, -NW_f, -NW_b, -NW_g, -NW_{nbp}, -NW_{ins}, -NW_{fnd}, -NW_{row}, 0 |  |  | Column-balancing row used for the paper-level presentation; it is not emitted as a runtime asset. |
| symbolic-tfm | Consumption | -C_h, +C_h, 0 |  | Household consumption [id: 32] |  |
| symbolic-tfm | Wages and household income | +W, -W, 0 |  | Household total income [id: 41] |  |
| symbolic-tfm | Taxes | -T_h, -T_f, -T_b, +T, 0 |  | Household PIT [id: 34]<br>Firm CIT [id: 42]<br>Government VAT revenue [id: 92]<br>Government excise revenue [id: 93]<br>Government customs duty revenue [id: 94]<br>Equity dividend tax [id: 56] |  |
| symbolic-tfm | Social contributions | -SC_h, -SC_f, +SC, 0 |  | ZUS contribution [id: 1]<br>NFZ contribution [id: 4]<br>PPK contribution [id: 7]<br>Labour Fund contribution [id: 9]<br>PFRON contribution [id: 12]<br>FGSP contribution [id: 15] |  |
| symbolic-tfm | Transfers and benefits | +TR_h, -TR_g, -TR_f, 0 |  | ZUS pension payment [id: 2]<br>Government unemployment benefits [id: 24]<br>Government social transfers [id: 25] |  |
| symbolic-tfm | Government spending | +G, -G, 0 |  | Government purchases [id: 21]<br>Government capital investment [id: 23]<br>NFZ health spending [id: 5]<br>Labour Fund spending [id: 10]<br>PFRON spending [id: 13]<br>FGSP spending [id: 16]<br>JST spending [id: 19] |  |
| symbolic-tfm | Insurance premiums and claims | -P+CL, +P-CL, 0 |  | Life insurance premium [id: 27]<br>Non-life insurance premium [id: 28]<br>Life insurance claim [id: 29]<br>Non-life insurance claim [id: 30]<br>Insurance investment income [id: 31] |  |
| symbolic-tfm | Loan interest | -rL_h, -rL_f, +rL, 0 |  | Household debt service [id: 35]<br>Firm interest paid [id: 45]<br>Mortgage interest [id: 64]<br>Bank firm-loan interest [id: 76] |  |
| symbolic-tfm | Deposit and reserve interest | +rD_h, -rD+rR, -rR, 0 |  | Household deposit interest [id: 36]<br>Bank reserve interest [id: 84]<br>Bank standing facility [id: 85]<br>Bank interbank interest [id: 86] |  |
| symbolic-tfm | Bond coupons | -iB_c, +iB_b, -iB_g, +iB_i, +iB_f, +iB_{row}, 0 |  | Government debt service [id: 22]<br>Bank government-bond income [id: 77]<br>Corporate bond coupon [id: 58]<br>Bank corporate-bond coupon [id: 95] |  |
| symbolic-tfm | Dividends | +Div_h, -Div, +Div_g, +Div_i, +Div_f, +Div_{row}, 0 |  | Domestic equity dividend [id: 54]<br>Foreign equity dividend [id: 55]<br>Government equity dividend [id: 89] |  |
| symbolic-tfm | External trade and income | +REM, +X-M, +EU, -NX-REM-EU, 0 |  | Trade exports [id: 66]<br>Trade imports [id: 67]<br>Tourism export [id: 68]<br>Tourism import [id: 69]<br>Primary income [id: 72]<br>EU funds [id: 73]<br>Diaspora inflow [id: 74]<br>Household remittance outflow [id: 37] |  |
| symbolic-tfm | Loan origination | +dL_h, +dL_f, -dL, 0 | Firm loans (FirmLoan)<br>Consumer loans (ConsumerLoan)<br>Mortgage loans (MortgageLoan)<br>NBFI loans (NbfiLoan) | Consumer credit origination [id: 38]<br>Firm new loan [id: 44]<br>Mortgage origination [id: 62]<br>NBFI loan origination [id: 104]<br>Quasi-fiscal lending [id: 110] |  |
| symbolic-tfm | Loan repayment and defaults | -repL_h, -repL_f, +repL, 0 | Firm loans (FirmLoan)<br>Consumer loans (ConsumerLoan)<br>Mortgage loans (MortgageLoan)<br>NBFI loans (NbfiLoan) | Consumer credit debt service [id: 39]<br>Consumer credit default [id: 40]<br>Firm loan repayment [id: 43]<br>Firm NPL default [id: 50]<br>Mortgage repayment [id: 63]<br>Mortgage default [id: 65]<br>NBFI repayment [id: 105]<br>NBFI default [id: 106]<br>Quasi-fiscal repayment [id: 111] |  |
| symbolic-tfm | Bond issuance and purchases | +dB_c, -dB_b, +dB_g, -dB_{nbp}, -dB_i, -dB_f, -dB_{row}, 0 | Government bonds (GovBondHTM)<br>Government bonds AFS (GovBondAFS)<br>Quasi-fiscal bonds (QuasiFiscalBond)<br>Corporate bonds (CorpBond) | Government bond primary market [id: 97]<br>Foreign government bond purchase [id: 98]<br>NBP QE government bond purchase [id: 99]<br>Insurance government bond purchase [id: 100]<br>TFI government bond purchase [id: 101]<br>Quasi-fiscal bond issuance [id: 107]<br>Quasi-fiscal bond amortization [id: 108]<br>Quasi-fiscal NBP absorption [id: 109]<br>Quasi-fiscal NBP bond amortization [id: 114]<br>Corporate bond issuance [id: 60]<br>Corporate bond amortization [id: 61] |  |
| symbolic-tfm | Deposit change | -dD_h, -dD_f, +dD, -dD_{fnd}, 0 | Demand deposits (DemandDeposit)<br>Term deposits (TermDeposit) | Investment deposit settlement [id: 102]<br>TFI deposit drain [id: 103]<br>Quasi-fiscal lending deposit [id: 112]<br>Quasi-fiscal repayment deposit [id: 113] |  |
