package io.github.aaronreidsmith

import scala.io.Source

abstract class Solution(year: Int, day: Int) extends Runnable {
  type I  // Input type
  type O1 // Output type of part 1
  type O2 // Output type of part 2

  // We want these the be "implemented" so if something doesn't have an input to be parsed or doesn't have a part 2
  // (such as day 25), it doesn't throw an error
  protected def parseInput(file: Source): I = ???
  protected def part1(input: I): O1         = ???
  protected def part2(input: I): O2         = ???

  // Helper entrypoint so anything that implements the `Solution` class can also be run individually
  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    println(s"Year $year, Day $day")
    val input = using(f"$year/day$day%02d.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    if (day < 25) {
      println(s"Part 2: ${part2(input)}")
    }
    println()
  }

  // Helper function to see if we are in a test environment
  def isTest: Boolean = Option(System.getenv("IS_TEST")).fold(false)(_.toBooleanOption.contains(true))
}
