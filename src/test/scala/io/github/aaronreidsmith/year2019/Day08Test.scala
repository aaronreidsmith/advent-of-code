package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

class Day08Test extends BaseTest {
  val suite: Suite = Suite(
    fileInput,
    1950,
    // Multiline strings and `stripMargin` removes extra space that is in real output, so we use this
    Seq(
      "",
      "#### #  #  ##  #  # #    ",
      "#    # #  #  # #  # #    ",
      "###  ##   #  # #### #    ",
      "#    # #  #### #  # #    ",
      "#    # #  #  # #  # #    ",
      "#    #  # #  # #  # #### "
    ).mkString("\n")
  )
}
