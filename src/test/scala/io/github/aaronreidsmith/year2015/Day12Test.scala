package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day12Test extends BaseTest {
  val suite: Suite = Suite(
    Seq(
      "[1,2,3]",
      """{"a":2,"b":4}""",
      "[[[3]]]",
      """{"a":{"b":4},"c":-1}""",
      """{"a":[-1,1]}""",
      """[-1,{"a":1}]""",
      "[]",
      "{}"
    ),
    Seq(6, 6, 3, 3, 0, 0, 0, 0),
    Seq(
      "[1,2,3]",
      """[1,{"c":"red","b":2},3]""",
      """{"d":"red","e":[1,2,3,4],"f":5}""",
      """[1,"red",5]"""
    ),
    Seq(6, 4, 0, 6)
  )
}
