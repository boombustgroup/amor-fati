package com.boombustgroup.amorfati.montecarlo

/** Shared CSV row contract used by Monte Carlo output schemas. */
private[montecarlo] final case class McCsvSchema[-Row](
    header: String,
    render: Row => String,
):
  def contramap[Source](f: Source => Row): McCsvSchema[Source] =
    McCsvSchema(
      header = header,
      render = source => render(f(source)),
    )
