package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{Point, Solution, using}

import scala.annotation.tailrec
import scala.io.Source

object Day03 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2015, Day 3")
    val input = using("2015/day03.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2015] def parseInput(file: Source): String = file.mkString

  override protected[year2015] def part1(instructions: String): Int = {
    val (_, seenHouses) = instructions.foldLeft(Point.ZERO, Set(Point.ZERO)) {
      case ((point, seen), char) =>
        val nextPos = nextPosition(point, char)
        (nextPos, seen + nextPos)
      case (acc, _) => acc
    }
    seenHouses.size
  }

  override protected[year2015] def part2(instructions: String): Int = {
    @tailrec
    def helper(
        pointer: Int = 0,
        santaPos: Point = Point.ZERO,
        santaSeen: Set[Point] = Set(Point.ZERO),
        robotSantaPos: Point = Point.ZERO,
        robotSantaSeen: Set[Point] = Set(Point.ZERO)
    ): Int = if (pointer >= instructions.length) {
      (santaSeen ++ robotSantaSeen).size
    } else {
      val char = instructions(pointer)
      if (pointer % 2 == 0) {
        val newSantaPos = nextPosition(santaPos, char)
        helper(pointer + 1, newSantaPos, santaSeen + newSantaPos, robotSantaPos, robotSantaSeen)
      } else {
        val newRobotSantaPos = nextPosition(robotSantaPos, char)
        helper(pointer + 1, santaPos, santaSeen, newRobotSantaPos, robotSantaSeen + newRobotSantaPos)
      }
    }

    helper()
  }

  private def nextPosition(point: Point, char: Char): Point = char match {
    case '^' => point.copy(x = point.x - 1)
    case '>' => point.copy(y = point.y + 1)
    case 'v' => point.copy(x = point.x + 1)
    case '<' => point.copy(y = point.y - 1)
    case _   => throw new IllegalArgumentException
  }
}
