package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.BaseTest

class Day14Test extends BaseTest {
  val suite: Suite = Suite(
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      |mem[8] = 11
      |mem[7] = 101
      |mem[8] = 0
      |""".stripMargin.parsed,
    165L,
    """mask = 000000000000000000000000000000X1001X
      |mem[42] = 100
      |mask = 00000000000000000000000000000000X0XX
      |mem[26] = 1
      |""".stripMargin.parsed,
    208L
  )
}
