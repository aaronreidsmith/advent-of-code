package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day02Test extends BaseTest {
  val suite: Suite = Suite(
    s"""5\t1\t9\t5
       |7\t5\t3
       |2\t4\t6\t8""".stripMargin.parsed,
    18,
    s"""5\t9\t2\t8
       |9\t4\t7\t3
       |3\t8\t6\t5""".stripMargin.parsed,
    9
  )
}
