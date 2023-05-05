package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day19Test extends BaseTest {
  val suite: Suite = Suite(
    """H => HO
      |H => OH
      |O => HH
      |
      |HOH""".stripMargin.parsed,
    4,
    fileInput,
    195
  )
}
