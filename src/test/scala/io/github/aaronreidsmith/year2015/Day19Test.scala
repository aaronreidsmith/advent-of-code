package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day19Test extends BaseTest {
  override val suite: Suite = Suite(
    Day19.parseInput(
      """H => HO
        |H => OH
        |O => HH
        |
        |HOH""".stripMargin.asSource
    ),
    4,
    fileInput,
    195
  )
}
