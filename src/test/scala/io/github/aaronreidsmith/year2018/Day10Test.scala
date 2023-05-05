package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day10Test extends BaseTest {
  val suite: Suite = Suite(
    fileInput,
    10244,
    // Have to do it this way because stripMargin removes the trailing spaces
    Seq(
      "",
      "  ##    #    #  ######   ####   #####   #    #  ######  ######",
      " #  #   #    #  #       #    #  #    #  #   #   #       #     ",
      "#    #  #    #  #       #       #    #  #  #    #       #     ",
      "#    #  #    #  #       #       #    #  # #     #       #     ",
      "#    #  ######  #####   #       #####   ##      #####   ##### ",
      "######  #    #  #       #  ###  #  #    ##      #       #     ",
      "#    #  #    #  #       #    #  #   #   # #     #       #     ",
      "#    #  #    #  #       #    #  #   #   #  #    #       #     ",
      "#    #  #    #  #       #   ##  #    #  #   #   #       #     ",
      "#    #  #    #  #        ### #  #    #  #    #  ######  ######",
      ""
    ).mkString("\n")
  )
}
