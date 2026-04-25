package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.PLN

object SimParamsTestOverrides:

  val pfronDeficit: SimParams =
    SimParams.defaults.copy(
      earmarked = SimParams.defaults.earmarked.copy(
        pfronMonthlyRevenue = PLN(1),
        pfronMonthlySpending = PLN(2),
      ),
    )

end SimParamsTestOverrides
