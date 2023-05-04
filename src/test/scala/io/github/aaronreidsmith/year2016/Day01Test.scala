package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day01Test extends BaseTest {
  override val suite: Suite = Suite(
    Seq("R2, L3", "R2, R2, R2", "R5, L5, R5, R3").map(str => Day01.parseInput(str.asSource)),
    Seq(5, 2, 12),
    Seq(Day01.parseInput("R8, R4, R4, R8".asSource)),
    Seq(4)
  )
}
