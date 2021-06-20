package io.github.aaronreidsmith.year2017

import scala.annotation.tailrec
import scala.io.Source

object Day11 {
  private var globalMaxStep: Option[Int] = None

  def main(args: Array[String]): Unit = {
    val input      = Source.fromResource("2017/day11.txt")
    val directions = input.mkString.split(',').toList
    input.close()

    println(s"Part 1: ${solution(directions)}")
    println(s"Part 2: ${globalMaxStep.get}")
  }

  @tailrec
  def solution(directions: List[String], x: Int = 0, y: Int = 0, z: Int = 0): Int = directions match {
    case Nil => distance(x, y, z)
    case currentDirection :: remainingDirections =>
      val (newX, newY, newZ) = currentDirection match {
        case "n"  => (x, y + 1, z - 1)
        case "ne" => (x + 1, y, z - 1)
        case "se" => (x + 1, y - 1, z)
        case "s"  => (x, y - 1, z + 1)
        case "sw" => (x - 1, y, z + 1)
        case "nw" => (x - 1, y + 1, z)
        case _    => throw new IllegalArgumentException
      }
      val currentCubeDistance = distance(newX, newY, newZ)
      globalMaxStep = globalMaxStep match {
        case Some(value) => if (currentCubeDistance > value) Some(currentCubeDistance) else globalMaxStep
        case None        => Some(currentCubeDistance)
      }
      solution(remainingDirections, newX, newY, newZ)
  }

  private def distance(x: Int, y: Int, z: Int): Int = Seq(math.abs(x), math.abs(y), math.abs(z)).max
}
