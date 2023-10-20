package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day07 extends Solution {
  type I  = IntCode
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): IntCode = IntCode(file)

  override def part1(input: IntCode): Long = {
    (0L to 4L).permutations.foldLeft(Long.MinValue) { (currentMax, perm) =>
      val thrustSignal = perm.foldLeft(0L)((acc, next) => input.withInput(next, acc).allOutput.last)
      currentMax.max(thrustSignal)
    }
  }

  override def part2(input: IntCode): Long = {
    @tailrec
    def helper(amps: Seq[IntCode], previousOutput: Long = 0L): Long = {
      val nextAmp = amps.head.withAdditionalInput(previousOutput).nextOutput
      nextAmp.result match {
        case IntCode.State.Output(nextOutput) => helper(amps.tail :+ nextAmp, nextOutput)
        case _                          => previousOutput
      }
    }

    (5L to 9L).map(input.withAdditionalInput(_)).permutations.map(helper(_)).max
  }
}
