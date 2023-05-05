package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

class Day09Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("1102,34915192,34915192,7,4,7,99,0", "104,1125899906842624,99").parsed,
    Seq(1219070632396864L, 1125899906842624L),
    Seq(fileInput),
    Seq(66113L)
  )
}
