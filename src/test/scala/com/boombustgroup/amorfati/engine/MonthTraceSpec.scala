package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.types.Share
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MonthTraceSpec extends AnyFlatSpec with Matchers:

  "MonthTimingTrace" should "reject duplicate envelope keys" in {
    val err = intercept[IllegalArgumentException]:
      MonthTimingTrace(
        Vector(
          MonthTimingEnvelope(MonthTimingEnvelopeKey.LaborSignals, MonthTimingPayload.LaborSignals(Share(0.4))),
          MonthTimingEnvelope(MonthTimingEnvelopeKey.LaborSignals, MonthTimingPayload.LaborSignals(Share(0.6))),
        ),
      )

    err.getMessage should include("unique envelope keys")
    err.getMessage should include(MonthTimingEnvelopeKey.LaborSignals.toString)
  }

  it should "fail clearly when a required payload is missing" in {
    val err = intercept[IllegalStateException]:
      MonthTimingTrace(Vector.empty).laborSignals

    err.getMessage should include("LaborSignals")
    err.getMessage should include(MonthTimingEnvelopeKey.LaborSignals.toString)
  }
