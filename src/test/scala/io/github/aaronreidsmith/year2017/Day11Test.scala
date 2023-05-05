package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day11Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("ne,ne,ne", "ne,ne,sw,sw", "ne,ne,s,s", "se,sw,se,sw,sw").parsed,
    Seq(3, 0, 2, 3),
    Seq(3, 3, 3, 3)
  )
}
