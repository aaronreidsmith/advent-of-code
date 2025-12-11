package io.github.aaronreidsmith.year2025

import io.github.aaronreidsmith.BaseTest

class Day11Test extends BaseTest {
  val suite: Suite = Suite(
    """aaa: you hhh
      |you: bbb ccc
      |bbb: ddd eee
      |ccc: ddd eee fff
      |ddd: ggg
      |eee: out
      |fff: out
      |ggg: out
      |hhh: ccc fff iii
      |iii: out""".stripMargin.parsed,
    5,
    """svr: aaa bbb
      |aaa: fft
      |fft: ccc
      |bbb: tty
      |tty: ccc
      |ccc: ddd eee
      |ddd: hub
      |hub: fff
      |eee: dac
      |dac: fff
      |fff: ggg hhh
      |ggg: out
      |hhh: out""".stripMargin.parsed,
    2L
  )
}
