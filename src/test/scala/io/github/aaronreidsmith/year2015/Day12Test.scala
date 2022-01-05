package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day12Test extends BaseTest {
  "Day12.part1" should "work on example input" in {
    Day12.part1("[1,2,3]") shouldBe 6
    Day12.part1("""{"a":2,"b":4}""") shouldBe 6
    Day12.part1("[[[3]]]") shouldBe 3
    Day12.part1("""{"a":{"b":4},"c":-1}""") shouldBe 3
    Day12.part1("""{"a":[-1,1]}""") shouldBe 0
    Day12.part1("""[-1,{"a":1}]""") shouldBe 0
    Day12.part1("[]") shouldBe 0
    Day12.part1("{}") shouldBe 0
  }

  "Day12.part2" should "work on example input" in {
    Day12.part2("[1,2,3]") shouldBe 6
    Day12.part2("""[1,{"c":"red","b":2},3]""") shouldBe 4
    Day12.part2("""{"d":"red","e":[1,2,3,4],"f":5}""") shouldBe 0
    Day12.part2("""[1,"red",5]""") shouldBe 6
  }
}
