package com.boombustgroup.amorfati.random

/** Stable seed splitting from one explicit root seed. */
object SeedDerivation:

  def derive(rootSeed: Long, keySalt: Long): Long =
    mix64(rootSeed ^ mix64(keySalt + 1L))

  def mix64(input: Long): Long =
    var z = input + 0x9e3779b97f4a7c15L
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)
