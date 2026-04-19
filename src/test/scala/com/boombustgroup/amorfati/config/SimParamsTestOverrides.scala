package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.PLN

object SimParamsTestOverrides:

  val pfronDeficit: SimParams =
    SimParams.defaults.copy(
      earmarked = SimParams.defaults.earmarked.copy(
        pfronMonthlyRevenue = PLN(1.0),
        pfronMonthlySpending = PLN(2.0),
      ),
    )

end SimParamsTestOverrides
