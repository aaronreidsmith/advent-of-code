package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day06Test extends BaseTest {
  val suite: Suite = Suite(
    // Turn on 1M lights, then turn off 1000 lights, then turn off 4 lights
    """turn on 0,0 through 999,999
      |toggle 0,0 through 999,0
      |turn off 499,499 through 500,500""".stripMargin.parsed,
    1000000 - 1000 - 4,
    // Increase brightness by 1 then increase brightness by 2M
    """turn on 0,0 through 0,0
      |toggle 0,0 through 999,999""".stripMargin.parsed,
    2000001
  )
}
