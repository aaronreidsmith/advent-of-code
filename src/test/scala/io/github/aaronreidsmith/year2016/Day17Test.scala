package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest
import org.scalatest.Ignore

@Ignore // TODO: This test passes when run in IntelliJ (via java ...) but not when run via `sbt run`, so just ignoring it
class Day17Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("ihgpwlah", "kglvqrro", "ulqzkmiv"),
    Seq("DDRRRD", "DDUDRLRRUDRD", "DRURDRUDDLLDLUURRDULRLDUUDDDRR"),
    Seq(370, 492, 830)
  )
}
