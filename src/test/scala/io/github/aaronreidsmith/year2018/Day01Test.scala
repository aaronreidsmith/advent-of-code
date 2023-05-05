package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day01Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("+1\n+1\n+1", "+1\n+1\n-2", "-1\n-2\n-3").parsed,
    Seq(3, 0, -6),
    Seq("+1\n-1", "+3\n+3\n+4\n-2\n-4", "-6\n+3\n+8\n+5\n-6", "+7\n+7\n-2\n-7\n-4").parsed,
    Seq(0, 10, 5, 14)
  )
}
