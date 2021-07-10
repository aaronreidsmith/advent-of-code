package io.github.aaronreidsmith.year2016

import scala.io.Source

object Day01 {
  private val left  = "^L(\\d+)$".r
  private val right = "^R(\\d+)$".r

  protected[this] object Direction extends Enumeration {
    type Direction = Value
    val North, East, South, West = Value
  }
  import Direction._

  def main(args: Array[String]): Unit = {
    val input        = Source.fromResource("2016/day01.txt")
    val instructions = input.mkString.split(", ").toList
    input.close()

    val ((finalRow, finalCol), _, visited) = instructions.foldLeft(((0, 0), North, List.empty[(Int, Int)])) {
      case (((currentRow, currentCol), currentDirection, visitedAcc), instruction) =>
        instruction match {
          case left(steps) if currentDirection == North =>
            val newCol     = currentCol - steps.toInt
            val newVisited = (currentCol until newCol by -1).map((currentRow, _))
            ((currentRow, newCol), West, visitedAcc ++ newVisited)
          case left(steps) if currentDirection == East =>
            val newRow     = currentRow + steps.toInt
            val newVisited = (currentRow until newRow).map((_, currentCol))
            ((newRow, currentCol), North, visitedAcc ++ newVisited)
          case left(steps) if currentDirection == South =>
            val newCol     = currentCol + steps.toInt
            val newVisited = (currentCol until newCol).map((currentRow, _))
            ((currentRow, newCol), East, visitedAcc ++ newVisited)
          case left(steps) if currentDirection == West =>
            val newRow     = currentRow - steps.toInt
            val newVisited = (currentRow until newRow by -1).map((_, currentCol))
            ((newRow, currentCol), South, visitedAcc ++ newVisited)
          case right(steps) if currentDirection == North =>
            val newCol     = currentCol + steps.toInt
            val newVisited = (currentCol until newCol).map((currentRow, _))
            ((currentRow, newCol), East, visitedAcc ++ newVisited)
          case right(steps) if currentDirection == East =>
            val newRow     = currentRow - steps.toInt
            val newVisited = (currentRow until newRow by -1).map((_, currentCol))
            ((newRow, currentCol), South, visitedAcc ++ newVisited)
          case right(steps) if currentDirection == South =>
            val newCol     = currentCol - steps.toInt
            val newVisited = (currentCol until newCol by -1).map((currentRow, _))
            ((currentRow, newCol), West, visitedAcc ++ newVisited)
          case right(steps) if currentDirection == West =>
            val newRow     = currentRow + steps.toInt
            val newVisited = (currentRow until newRow).map((_, currentCol))
            ((newRow, currentCol), North, visitedAcc ++ newVisited)
          case other => throw new IllegalArgumentException(other)
        }
    }
    val part1 = math.abs(finalRow) + math.abs(finalCol)
    println(s"Part 1: $part1")

    val (part2, _) = visited.foldLeft(0, Set.empty[(Int, Int)]) {
      case ((coordinateDistance, visitedSet), coordinate) if coordinateDistance == 0 =>
        if (visitedSet.contains(coordinate)) {
          (math.abs(coordinate._1) + math.abs(coordinate._2), visitedSet)
        } else {
          (0, visitedSet + coordinate)
        }
      case ((answer, visitedSet), _) => (answer, visitedSet)
    }
    println(s"Part 2: $part2")
  }
}
