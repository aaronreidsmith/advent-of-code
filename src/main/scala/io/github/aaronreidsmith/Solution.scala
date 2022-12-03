package io.github.aaronreidsmith

import scala.io.Source

trait Solution extends Runnable {
  type I  // Input type
  type O1 // Output type of part 1
  type O2 // Output type of part 2

  // We want these the be "implemented" so if something doesn't have an input to be parsed or doesn't have a part 2
  // (such as day 25), it doesn't throw an error
  protected def parseInput(file: Source): I = ???
  protected def part1(input: I): O1         = ???
  protected def part2(input: I): O2         = ???

  // Helper entrypoint so anything that implements the `Solution` trait can also be run individually
  def main(args: Array[String]): Unit = {
    run()
  }

  // Helper function to see if we are in a test environment
  def isTest: Boolean = Option(System.getenv("IS_TEST")).fold(false)(_.toBooleanOption.contains(true))
}
