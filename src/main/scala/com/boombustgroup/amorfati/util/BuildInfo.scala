package com.boombustgroup.amorfati.util

import java.util.Properties

object BuildInfo:
  val gitCommit: String =
    val props = new Properties()
    val is    = Option(getClass.getResourceAsStream("/version.properties"))
    is.foreach(props.load)
    is.foreach(_.close())
    props.getProperty("git.commit", "unknown")
