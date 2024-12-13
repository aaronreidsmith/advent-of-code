package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

object Day13 extends Solution {
  type I  = List[Game]
  type O1 = Long
  type O2 = Long

  case class Button(x: Int, y: Int)
  case class Game(a: Button, b: Button, prize: Point)

  private val gameRegex = """Button A: X\+(\d+), Y\+(\d+)
                            |Button B: X\+(\d+), Y\+(\d+)
                            |Prize: X=(\d+), Y=(\d+)""".stripMargin.r

  override def parseInput(file: Source): List[Game] = {
    file.mkString
      .split("\n\n")
      .collect {
        case gameRegex(aX, aY, bX, bY, prizeX, prizeY) =>
          Game(Button(aX.toInt, aY.toInt), Button(bX.toInt, bY.toInt), Point(prizeX.toInt, prizeY.toInt))
      }
      .toList
  }

  override def part1(input: List[Game]): Long = {
    input.foldLeft(0) {
      case (acc, Game(a, b, prize)) =>
        acc + {
          for {
            aPresses <- 0 to 100
            bPresses <- 0 to 100
            if (a.x * aPresses + b.x * bPresses) == prize.x && (a.y * aPresses + b.y * bPresses) == prize.y
          } yield (3 * aPresses) + bPresses
        }.minOption.getOrElse(0)
    }
  }

  override def part2(input: List[Game]): Long = {
    input.foldLeft(0L) {
      case (acc, Game(a, b, prize)) =>
        val targetX = 10000000000000L + prize.x
        val targetY = 10000000000000L + prize.y
        // a.x * aPresses + b.x * bPresses = targetX, so we can rearrange to find aPresses. Similar for b
        val bPresses = (targetX * a.y - targetY * a.x) / (a.y.toLong * b.x.toLong - b.y.toLong * a.x.toLong)
        val aPresses = (targetX * b.y - targetY * b.x) / (b.y.toLong * a.x.toLong - a.y.toLong * b.x.toLong)
        if (
          (a.x.toLong * aPresses + b.x.toLong * bPresses == targetX) &&
          (a.y.toLong * aPresses + b.y.toLong * bPresses == targetY)
        ) {
          acc + (aPresses * 3) + bPresses
        } else {
          acc
        }
    }
  }
}
