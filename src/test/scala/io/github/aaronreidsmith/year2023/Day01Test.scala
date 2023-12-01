package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.BaseTest

class Day01Test extends BaseTest {
  val suite: Suite = Suite(
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin.parsed,
    142,
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin.parsed,
    281
  )
}
