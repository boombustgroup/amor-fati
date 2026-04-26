package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Earmarked funds configuration: FP, PFRON, FGŚP.
  *
  * @param fpRate
  *   Fundusz Pracy employer contribution rate (2.45%, Ustawa o promocji
  *   zatrudnienia Art. 104)
  * @param fpAlmpSpendPerWorker
  *   monthly ALMP spending per employed worker (training, job placement)
  * @param pfronMonthlyRevenue
  *   PFRON monthly revenue from employer levies (~460 mln PLN/mo, PFRON 2024)
  * @param pfronMonthlySpending
  *   PFRON monthly disability spending (~420 mln PLN/mo, PFRON 2024)
  * @param fgspRate
  *   FGŚP payroll contribution rate (0.10%, Ustawa o ochronie roszczeń
  *   pracowniczych)
  * @param fgspPayoutPerWorker
  *   average FGŚP payout per worker at bankrupt firm (3 months unpaid wages
  *   cap)
  */
case class EarmarkedConfig(
    fpRate: Rate = Rate.decimal(245, 4),
    fpAlmpSpendPerWorker: PLN = PLN(15),
    pfronMonthlyRevenue: PLN = PLN(460000000),
    pfronMonthlySpending: PLN = PLN(420000000),
    fgspRate: Rate = Rate.decimal(1, 3),
    fgspPayoutPerWorker: PLN = PLN(10000),
)
