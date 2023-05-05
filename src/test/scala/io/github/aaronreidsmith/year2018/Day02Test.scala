package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day02Test extends BaseTest {
  val suite: Suite = Suite(
    """abcdef
      |bababc
      |abbcde
      |abcccd
      |aabcdd
      |abcdee
      |ababab""".stripMargin.parsed,
    12,
    """abcde
      |fghij
      |klmno
      |pqrst
      |fguij
      |axcye
      |wvxyz""".stripMargin.parsed,
    "fgij"
  )
}
