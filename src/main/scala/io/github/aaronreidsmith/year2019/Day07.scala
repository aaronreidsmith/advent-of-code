package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day07 extends Solution(2019, 7) {
  type I  = IntCode
  type O1 = Long
  type O2 = Long

  override protected[year2019] def parseInput(file: Source): IntCode = IntCode(file)

  override protected[year2019] def part1(input: IntCode): Long = {
    (0L to 4L).permutations.foldLeft(Long.MinValue) { (currentMax, perm) =>
      val thrustSignal = perm.foldLeft(0L)((acc, next) => input.withInput(next, acc).allOutput.last)
      currentMax.max(thrustSignal)
    }
  }

  override protected[year2019] def part2(input: IntCode): Long = {
    @tailrec
    def helper(amps: Seq[IntCode], previousOutput: Long = 0L): Long = {
      val nextAmp = amps.head.withInput(previousOutput).nextOutput
      nextAmp.result match {
        case IntCode.Output(nextOutput) => helper(amps.tail :+ nextAmp, nextOutput)
        case _                          => previousOutput
      }
    }

    (5L to 9L).map(input.withInput(_)).permutations.map(helper(_)).max
  }
}
