package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day01Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("R2, L3", "R2, R2, R2", "R5, L5, R5, R3").parsed,
    Seq(5, 2, 12),
    Seq("R8, R4, R4, R8").parsed,
    Seq(4)
  )
}
