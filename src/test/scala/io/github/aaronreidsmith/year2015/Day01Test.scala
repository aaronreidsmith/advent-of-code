package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day01Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("(())", "()()", "(((", "(()(()(", "))(((((", "())", "))(", ")))", ")())())"),
    Seq(0, 0, 3, 3, 3, -1, -1, -3, -3),
    Seq(")", "()())"),
    Seq(1, 5)
  )
}
