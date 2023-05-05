package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day04Test extends BaseTest {
  val suite: Suite = Suite(
    """aa bb cc dd ee
      |aa bb cc dd aa
      |aa bb cc dd aaa""".stripMargin.parsed,
    2,
    """abcde fghij
      |abcde xyz ecdab
      |a ab abc abd abf abj
      |iiii oiii ooii oooi oooo
      |oiii ioii iioi iiio""".stripMargin.parsed,
    3
  )
}
