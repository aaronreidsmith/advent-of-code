package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day01Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("1122", "1111", "1234", "91212129").parsed,
    Seq(3, 4, 0, 9),
    Seq("1212", "1221", "123424", "123123", "12131415").parsed,
    Seq(6, 0, 4, 12, 4)
  )
}
