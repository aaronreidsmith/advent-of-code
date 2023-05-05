package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day09Test extends BaseTest {
  val suite: Suite = Suite(
    Seq((10, 1618), (13, 7999), (17, 1104), (21, 6111), (30, 5807)),
    Seq(8317L, 146373L, 2764L, 54718L, 37305L),
    Seq(fileInput),
    Seq(3314195047L)
  )
}
