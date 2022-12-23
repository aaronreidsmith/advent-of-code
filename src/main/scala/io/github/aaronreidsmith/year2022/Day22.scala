package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Direction, East, Grid, North, Point, Solution, South, West}

import scala.annotation.tailrec
import scala.io.Source

object Day22 extends Solution(2022, 22) {
  type I  = (Grid[Char], List[Instruction])
  type O1 = Int
  type O2 = Int

  private[year2022] sealed trait Instruction
  private case object Left            extends Instruction
  private case object Right           extends Instruction
  private case class Move(steps: Int) extends Instruction

  private implicit class DirectionOps(direction: Direction) {
    def toInt: Int = direction match {
      case East  => 0
      case South => 1
      case West  => 2
      case North => 3
    }
  }

  override protected[year2022] def parseInput(file: Source): (Grid[Char], List[Instruction]) = {
    val Array(rawGrid, rawInstructions, _*) = file.mkString.stripTrailing().split("\n\n")

    val grid = {
      for {
        (line, row) <- rawGrid.split('\n').zipWithIndex
        (char, col) <- line.zipWithIndex
        if char != ' '
      } yield Point(row + 1, col + 1) -> char // This grid is 1-indexed
    }.toMap

    val moves = rawInstructions.split("[LR]").map(num => Move(num.toInt)).toList
    val turns = rawInstructions
      .split("\\d+")
      .collect {
        case "L" => Left
        case "R" => Right
      }
      .toList
    // https://stackoverflow.com/a/24221727/10696164
    val interleaved = moves
      .grouped(1)
      .zipAll(turns.grouped(1), Nil, Nil)
      .filterNot(_._1.isEmpty)
      .flatMap(x => x._1 ++ x._2)
      .toList

    (grid, interleaved)
  }

  override protected[year2022] def part1(input: (Grid[Char], List[Instruction])): Int = {
    val (grid, initialInstructions) = input
    solution(
      grid,
      initialInstructions,
      (point, direction) => {
        val sameRow = grid.keys.filter(_.x == point.x)
        val sameCol = grid.keys.filter(_.y == point.y)
        val wrappedPoint = direction match {
          case North => sameCol.maxBy(_.x)
          case East  => sameRow.minBy(_.y)
          case South => sameCol.minBy(_.x)
          case West  => sameRow.maxBy(_.y)
        }
        if (grid.contains(wrappedPoint) && grid(wrappedPoint) != '#') (wrappedPoint, direction) else (point, direction)
      }
    )
  }

  override protected[year2022] def part2(input: (Grid[Char], List[Instruction])): Int = {
    val (grid, initialInstructions) = input

    // Get our sections
    val sorted = grid.toSeq.sortBy(_._1)
    val AB     = sorted.take(5000).sortBy(_._1.y)
    val A      = AB.takeRight(2500).toMap       // (1,101) to (50,150)
    val B      = AB.take(2500).toMap            // (1,51) to (50,100)
    val C      = sorted.slice(5000, 7500).toMap // (51,51) to (100,100)
    val DE     = sorted.slice(7500, 12500).sortBy(_._1.y)
    val D      = DE.takeRight(2500).toMap       // (101,51) to (150,100)
    val E      = DE.take(2500).toMap            // (101,1) to (150,50)
    val F      = sorted.takeRight(2500).toMap   // (151,1) to (200,50)

    solution(
      grid,
      initialInstructions,
      /* Input looks like this:
       *
       *  BA
       *  C
       * ED
       * F
       *
       * Some of the rules will not be encountered because they are not technically a "wrap" (A<->B, B<->C, C<->D,
       * D<->E, E<->F), but I've included them for completeness
       */
      (point, direction) => {
        val (newPoint, newDirection) = if (A.contains(point)) {
          direction match {
            case North => (Point(200, point.y - 100), North) // Walk off "A" upward, end up on "F" facing up
            case East  => (Point(151 - point.x, 100), West)  // Walk off "A" to the right, end up on "D" facing left
            case South => (Point(point.y - 50, 100), West)   // Walk off "A" downward, end up on "C" facing left
            case West  => (point.move(direction), direction) // Walk off "A" to the left, end up on "B" facing left
          }
        } else if (B.contains(point)) {
          direction match {
            case North => (Point(point.y + 100, 1), East)    // Walk off "B" upward, end up on "F" facing right
            case East  => (point.move(direction), direction) // Walk off "B" to the right, end up on "A" facing right
            case South => (point.move(direction), direction) // Walk off "B" downward, end up on "C" facing down
            case West  => (Point(151 - point.x, 1), East)    // Walk off "B" to the left, end up on "E" facing right
          }
        } else if (C.contains(point)) {
          direction match {
            case North => (point.move(direction), direction) // Walk off "C" upward, end up on "B" facing up
            case East  => (Point(50, point.x + 50), North)   // Walk off "C" to the right, end up on "A" facing up
            case South => (point.move(direction), direction) // Walk off "C" downward, end up on "D" facing down
            case West  => (Point(101, point.x - 50), South)  // Walk off "C" to the left, end up on "E" facing down
          }
        } else if (D.contains(point)) {
          direction match {
            case North => (point.move(direction), direction) // Walk off "D" upward, end up on "C" facing up
            case East  => (Point(151 - point.x, 150), West)  // Walk off "D" to the right, end up on "A" facing left
            case South => (Point(point.y + 100, 50), West)   // Walk off "D" downward, end up on "F" facing left
            case West  => (point.move(direction), direction) // Walk off "D" to the left, end up on "E" facing left
          }
        } else if (E.contains(point)) {
          direction match {
            case North => (Point(point.y + 50, 51), East)    // Walk off "E" upward, end up on "C" facing right
            case East  => (point.move(direction), direction) // Walk off "E" to the right, end up on "D" facing right
            case South => (point.move(direction), direction) // Walk off "E" downward, end up on "F" facing down
            case West  => (Point(151 - point.x, 51), East)   // Walk off "E" to the left, end up on "B" facing right
          }
        } else if (F.contains(point)) {
          direction match {
            case North => (point.move(direction), direction) // Walk off "F" upward, end up on "E" facing up
            case East  => (Point(150, point.x - 100), North) // Walk off "F" to the right, end up on "D" facing up
            case South => (Point(1, point.y + 100), South)   // Walk off "F" downward, end up on "A" facing down
            case West  => (Point(1, point.x - 100), South)   // Walk off "F" to the left, end up on "B" facing down
          }
        } else {
          (point, direction)
        }

        if (grid.contains(newPoint) && grid(newPoint) != '#') (newPoint, newDirection) else (point, direction)
      }
    )
  }

  private def solution(
      grid: Grid[Char],
      initialInstructions: List[Instruction],
      wrap: (Point, Direction) => (Point, Direction)
  ): Int = {
    @tailrec
    def helper(position: Point, instructions: List[Instruction], direction: Direction = East): Int = {
      instructions match {
        case Nil           => (1000 * position.x) + (4 * position.y) + direction.toInt
        case Left :: tail  => helper(position, tail, direction.left)
        case Right :: tail => helper(position, tail, direction.right)
        case Move(steps) :: tail =>
          val (newPos, newDirection) = (0 until steps).foldLeft((position, direction)) {
            case ((currentPosition, currentDirection), _) =>
              val next = currentPosition.move(currentDirection)
              grid.get(next) match {
                case Some('#') => (currentPosition, currentDirection)     // Hit a wall, stop
                case Some('.') => (next, currentDirection)                // Open space, continue
                case _         => wrap(currentPosition, currentDirection) // Wrap based on input function
              }
            case (acc, _) => acc
          }
          helper(newPos, tail, newDirection)
      }
    }

    helper(grid.keys.min, initialInstructions)
  }
}
