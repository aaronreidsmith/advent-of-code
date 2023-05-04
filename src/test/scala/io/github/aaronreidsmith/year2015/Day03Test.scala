package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day03Test extends BaseTest {
  override val suite: Suite = Suite(
    Seq(">", "^>v<", "^v^v^v^v^v"),
    Seq(2, 4, 2),
    Seq("^v", "^>v<", "^v^v^v^v^v"),
    Seq(3, 3, 11)
  )
}
