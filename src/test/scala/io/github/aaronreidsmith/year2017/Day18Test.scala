package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day18Test extends BaseTest {
  val suite: Suite = Suite(
    """set a 1
      |add a 2
      |mul a a
      |mod a 5
      |snd a
      |set a 0
      |rcv a
      |jgz a -1
      |set a 1
      |jgz a -2""".stripMargin.parsed,
    4,
    """snd 1
      |snd 2
      |snd p
      |rcv a
      |rcv b
      |rcv c
      |rcv d""".stripMargin.parsed,
    3
  )
}
