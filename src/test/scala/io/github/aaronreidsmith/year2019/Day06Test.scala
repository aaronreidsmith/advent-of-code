package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

class Day06Test extends BaseTest {
  val suite: Suite = Suite(
    """COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L""".stripMargin.parsed,
    42,
    """COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L
      |K)YOU
      |I)SAN""".stripMargin.parsed,
    4
  )
}
