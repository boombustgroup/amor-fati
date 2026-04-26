package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Non-bank financial institutions (NBFI): TFI investment funds and shadow
  * banking credit.
  *
  * Models two NBFI channels: (1) TFI (Towarzystwa Funduszy Inwestycyjnych)
  * investment funds with ~380 mld PLN AUM, three-asset allocation across
  * government bonds, corporate bonds, and equities; (2) counter-cyclical NBFI
  * credit (~231 mld PLN stock) that acts as deposit drain from the banking
  * system. Calibrated to KNF/IZFiA 2024 data. Affects SFC Identities 2, 5, and 13.
  *
  * Stock values are in raw PLN — scaled by `gdpRatio` in `SimParams.defaults`.
  *
  * @param tfiInitAum
  *   initial TFI assets under management in raw PLN (IZFiA 2024: ~380 mld,
  *   scaled by gdpRatio)
  * @param tfiGovBondShare
  *   share of TFI AUM invested in government bonds
  * @param tfiCorpBondShare
  *   share of TFI AUM invested in corporate bonds
  * @param tfiEquityShare
  *   share of TFI AUM invested in equities (GPW)
  * @param tfiInflowRate
  *   monthly net inflow rate as fraction of AUM
  * @param tfiRebalanceSpeed
  *   monthly portfolio rebalancing speed toward target allocation
  * @param creditInitStock
  *   initial NBFI credit stock in raw PLN (KNF 2024: ~231 mld, scaled by
  *   gdpRatio)
  * @param creditBaseRate
  *   base monthly NBFI credit origination rate (fraction of stock)
  * @param creditRate
  *   NBFI lending rate (higher than bank rate due to risk profile)
  * @param countercyclical
  *   sensitivity of NBFI credit to bank credit tightening (counter-cyclical
  *   channel)
  * @param creditMaturity
  *   average NBFI loan maturity in months
  * @param defaultBase
  *   base monthly default rate on NBFI credit
  * @param defaultUnempSens
  *   sensitivity of NBFI defaults to unemployment rate
  */
case class NbfiConfig(
    tfiInitAum: PLN = PLN(380000000000L),      // raw — scaled by gdpRatio
    tfiGovBondShare: Share = Share.decimal(40, 2),
    tfiCorpBondShare: Share = Share.decimal(10, 2),
    tfiEquityShare: Share = Share.decimal(10, 2),
    tfiInflowRate: Share = Share.decimal(1, 3),
    tfiRebalanceSpeed: Coefficient = Coefficient.decimal(5, 2),
    creditInitStock: PLN = PLN(231000000000L), // raw — scaled by gdpRatio
    creditBaseRate: Share = Share.decimal(5, 3),
    creditRate: Rate = Rate.decimal(10, 2),
    countercyclical: Coefficient = Coefficient(2),
    creditMaturity: Scalar = Scalar(36),
    defaultBase: Share = Share.decimal(2, 3),
    defaultUnempSens: Coefficient = Coefficient(3),
)
