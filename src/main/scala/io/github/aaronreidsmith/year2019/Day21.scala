package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day21 extends Solution {
  type I  = IntCode
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): IntCode = IntCode(file)

  override def part1(input: IntCode): Long = solution(
    input,
    """NOT A T
      |OR T J
      |NOT B T
      |OR T J
      |NOT C T
      |OR T J
      |AND D J
      |WALK
      |""".stripMargin
  )

  override def part2(input: IntCode): Long = solution(
    input,
    """NOT A J
      |NOT B T
      |OR T J
      |NOT C T
      |OR T J
      |AND E T
      |OR H T
      |AND T J
      |AND D J
      |RUN
      |""".stripMargin
  )

  private def solution(intCode: IntCode, script: String): Long = {
    intCode.withInput(script.map(_.toLong): _*).allOutput.last
  }
}
