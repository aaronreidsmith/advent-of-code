package io.github

import scala.io.Source
import scala.util.Using

package object aaronreidsmith {
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

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)

    def immediateNeighbors: Seq[Point] = Seq(Point(x - 1, y), Point(x + 1, y), Point(x, y - 1), Point(x, y + 1))

    def neighbors: Seq[Point] = for {
      dx <- Seq(-1, 0, 1)
      dy <- Seq(-1, 0, 1)
      if (dx, dy) != (0, 0)
    } yield Point(x + dx, y + dy)

    def manhattanDistance(that: Point): Int = (this.x - that.x).abs + (this.y - that.y).abs

    def unzip: (Int, Int) = (x, y)
  }

  object Point {
    def ZERO: Point = Point(0, 0)
  }

  type Grid[T] = Map[Point, T]

  def using[T](resourceName: String)(body: Source => T): T = Using.resource(Source.fromResource(resourceName))(body)
}
