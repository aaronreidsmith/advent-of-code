package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.util.FileUtils
import io.github.aaronreidsmith.year2019.intcode.IntCode

import scala.io.Source

// Written after my Raku solution to make sure my IntCode trait works
object Day02 extends FileUtils {
  def main(args: Array[String]): Unit = {
    val instructions = using(Source.fromResource("2019/day02.txt"))(_.mkString.split(',').map(_.toInt))

    val part1Instructions = instructions.patch(1, Seq(12, 2), 2) // From problem
    val part1IntCode      = new IntCode(part1Instructions)
    println(s"Part 1: ${part1IntCode.run()}")

    val part2 = {
      for {
        noun <- 0 until 100
        verb <- 0 until 100
      } yield (noun, verb)
    }.find {
      case (noun, verb) =>
        val updatedInstructions = instructions.patch(1, Seq(noun, verb), 2)
        val currentIntCode      = new IntCode(updatedInstructions)
        currentIntCode.run() == 19690720 // From problem
    }.map {
      case (noun, verb) => (100 * noun) + verb
    }.get
    println(s"Part 2: $part2")
  }
}
