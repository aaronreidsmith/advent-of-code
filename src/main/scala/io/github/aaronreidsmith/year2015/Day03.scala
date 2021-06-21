package io.github.aaronreidsmith.year2015

import scala.annotation.tailrec
import scala.io.Source

object Day03 {
  def main(args: Array[String]): Unit = {
    val input       = Source.fromResource("2015/day03.txt")
    val inputString = input.mkString
    input.close()
    println(s"Part 1: ${part1(inputString)}")
    println(s"Part 2: ${part2(inputString)}")
  }

  private def part1(instructions: String): Int = {
    val (_, seenHouses) = instructions.foldLeft((0, 0), Set((0, 0))) {
      case (((x, y), seen), char) =>
        val nextPos = nextPosition(x, y, char)
        (nextPos, seen + nextPos)
    }
    seenHouses.size
  }

  @tailrec
  private def part2(
      instructions: String,
      pointer: Int = 0,
      santaPos: (Int, Int) = (0, 0),
      santaSeen: Set[(Int, Int)] = Set(),
      robotSantaPos: (Int, Int) = (0, 0),
      robotSantaSeen: Set[(Int, Int)] = Set()
  ): Int = if (pointer >= instructions.length) {
    (santaSeen ++ robotSantaSeen).size
  } else {
    val char = instructions(pointer)
    if (pointer % 2 == 0) {
      val newSantaPos = nextPosition(santaPos, char)
      part2(instructions, pointer + 1, newSantaPos, santaSeen + newSantaPos, robotSantaPos, robotSantaSeen)
    } else {
      val newRobotSantaPos = nextPosition(robotSantaPos, char)
      part2(instructions, pointer + 1, santaPos, santaSeen, newRobotSantaPos, robotSantaSeen + newRobotSantaPos)
    }
  }

  private def nextPosition(pos: (Int, Int), char: Char): (Int, Int) = nextPosition(pos._1, pos._2, char)

  private def nextPosition(x: Int, y: Int, char: Char): (Int, Int) = char match {
    case '^' => (x - 1, y)
    case '>' => (x, y + 1)
    case 'v' => (x + 1, y)
    case '<' => (x, y - 1)
    case _   => throw new IllegalArgumentException
  }
}
