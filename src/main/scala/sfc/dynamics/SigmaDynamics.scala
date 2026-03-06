package sfc.dynamics

object SigmaDynamics:
  /** Evolve per-sector sigma via Arthur-style increasing returns / learning-by-doing.
    *
    * sigma_s(t+1) = sigma_s(t) + lambda * sigma_s(t) * adoptionRate_s(t) * (1 - sigma_s(t)/cap)
    *
    * @param currentSigmas
    *   current per-sector sigma values
    * @param baseSigmas
    *   initial per-sector sigma (used to compute logistic cap)
    * @param sectorAdoption
    *   per-sector adoption rates (fraction automated+hybrid)
    * @param lambda
    *   learning rate (0.0 = static mode, no-op)
    * @param capMult
    *   logistic ceiling multiplier (cap = baseSigma * capMult)
    * @return
    *   updated sigma vector (never decreasing -- ratchet)
    */
  def evolve(
    currentSigmas: Vector[Double],
    baseSigmas: Vector[Double],
    sectorAdoption: Vector[Double],
    lambda: Double,
    capMult: Double,
  ): Vector[Double] =
    if lambda == 0.0 then currentSigmas
    else
      currentSigmas.zip(baseSigmas).zip(sectorAdoption).map { case ((sig, base), adopt) =>
        val cap = base * capMult
        val delta = lambda * sig * adopt * (1.0 - sig / cap)
        Math.min(cap, Math.max(sig, sig + delta)) // ratchet + hard cap
      }
