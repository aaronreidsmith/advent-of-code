package io.github.aaronreidsmith

import io.github.aaronreidsmith.annotations.Slow

import scala.io.Source

trait Solution extends Runnable {
  type I  // Input type
  type O1 // Output type of part 1
  type O2 // Output type of part 2

  // We want these the be "implemented" so if something doesn't have an input to be parsed or doesn't have a part 2
  // (such as day 25), it doesn't throw an error
  def parseInput(file: Source): I = ???
  def part1(input: I): O1         = ???
  def part2(input: I): O2         = ???

  // Used to have to pass these in, but since we have a normalized class structure, we can just grab it at run time
  protected def year: Int = getClass.getName.split("year")(1).take(4).toInt
  protected def day: Int  = getClass.getName.split("Day")(1).take(2).toInt

  // Helper function to see if we are in a test environment
  protected def isTest: Boolean = Option(System.getenv("IS_TEST")).fold(false)(_.toBooleanOption.contains(true))

  // Helper entrypoint so anything that implements the `Solution` trait can also be run individually
  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    println(s"Year $year, Day $day")

    // Check for "Slow" annotation. Do this first because parsing can be marked as slow
    val annotation = Option(getClass.getAnnotation(classOf[Slow]))
    annotation.foreach { a =>
      if (a.parsing()) {
        println("Parsing for this solution has been marked as slow; please be patient!")
      }
      if (a.part1()) {
        if (a.part2()) {
          println("Both parts of this solution have been marked as slow; please be patient!")
        } else {
          println("Part 1 of this solution has been marked as slow; please be patient!")
        }
      }
    }

    val input = using(f"$year/day$day%02d.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")

    if (day < 25) {
      annotation.foreach { a =>
        if (!a.part1() && a.part2()) {
          println("Part 2 of this solution has been marked as slow; please be patient!")
        }
      }
      println(s"Part 2: ${part2(input)}")
    }
    println()
  }
}
