package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.BaseTest

class Day10Test extends BaseTest {
  val suite: Suite = Suite(
    fileInput,
    13140,
    // Example output on website uses periods instead of spaces, but I switched it so I could read the actual output
    // for my given input. Have to use a seq/mkString instead of stripMargin or Scala strips trailing spaces
    Seq(
      "",
      "##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ",
      "###   ###   ###   ###   ###   ###   ### ",
      "####    ####    ####    ####    ####    ",
      "#####     #####     #####     #####     ",
      "######      ######      ######      ####",
      "#######       #######       #######     ",
      ""
    ).mkString("\n")
  )
}
