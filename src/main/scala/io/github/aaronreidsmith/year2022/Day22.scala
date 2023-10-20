package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.*

import scala.annotation.tailrec
import scala.io.Source

object Day22 extends Solution {
  type I  = (Grid[Char], List[Instruction])
  type O1 = Int
  type O2 = Int

  enum Instruction {
    case Left
    case Right
    case Move(val steps: Int)
  }

  extension (direction: Direction) {
    def toInt: Int = direction match {
      case Direction.East  => 0
      case Direction.South => 1
      case Direction.West  => 2
      case Direction.North => 3
    }
  }

  override def parseInput(file: Source): (Grid[Char], List[Instruction]) = {
    val Array(rawGrid, rawInstructions, _*) = file.mkString.stripTrailing().split("\n\n"): @unchecked

    val grid = {
      for {
        (line, row) <- rawGrid.split('\n').zipWithIndex
        (char, col) <- line.zipWithIndex
        if char != ' '
      } yield Point(row + 1, col + 1) -> char // This grid is 1-indexed
    }.toMap

    val moves = rawInstructions.split("[LR]").map(num => Instruction.Move(num.toInt)).toList
    val turns = rawInstructions
      .split("\\d+")
      .collect {
        case "L" => Instruction.Left
        case "R" => Instruction.Right
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

  override def part1(input: (Grid[Char], List[Instruction])): Int = {
    val (grid, initialInstructions) = input
    solution(
      grid,
      initialInstructions,
      (point, direction) => {
        val sameRow = grid.keys.filter(_.x == point.x)
        val sameCol = grid.keys.filter(_.y == point.y)
        val wrappedPoint = direction match {
          case Direction.North => sameCol.maxBy(_.x)
          case Direction.East  => sameRow.minBy(_.y)
          case Direction.South => sameCol.minBy(_.x)
          case Direction.West  => sameRow.maxBy(_.y)
        }
        if (grid.contains(wrappedPoint) && grid(wrappedPoint) != '#') (wrappedPoint, direction) else (point, direction)
      }
    )
  }

  override def part2(input: (Grid[Char], List[Instruction])): Int = {
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
            case Direction.North =>
              (Point(200, point.y - 100), Direction.North) // Walk off "A" upward, end up on "F" facing up
            case Direction.East =>
              (Point(151 - point.x, 100), Direction.West) // Walk off "A" to the right, end up on "D" facing left
            case Direction.South =>
              (Point(point.y - 50, 100), Direction.West) // Walk off "A" downward, end up on "C" facing left
            case Direction.West =>
              (point.move(direction), direction) // Walk off "A" to the left, end up on "B" facing left
          }
        } else if (B.contains(point)) {
          direction match {
            case Direction.North =>
              (Point(point.y + 100, 1), Direction.East) // Walk off "B" upward, end up on "F" facing right
            case Direction.East =>
              (point.move(direction), direction) // Walk off "B" to the right, end up on "A" facing right
            case Direction.South =>
              (point.move(direction), direction) // Walk off "B" downward, end up on "C" facing down
            case Direction.West =>
              (Point(151 - point.x, 1), Direction.East) // Walk off "B" to the left, end up on "E" facing right
          }
        } else if (C.contains(point)) {
          direction match {
            case Direction.North => (point.move(direction), direction) // Walk off "C" upward, end up on "B" facing up
            case Direction.East =>
              (Point(50, point.x + 50), Direction.North) // Walk off "C" to the right, end up on "A" facing up
            case Direction.South =>
              (point.move(direction), direction) // Walk off "C" downward, end up on "D" facing down
            case Direction.West =>
              (Point(101, point.x - 50), Direction.South) // Walk off "C" to the left, end up on "E" facing down
          }
        } else if (D.contains(point)) {
          direction match {
            case Direction.North => (point.move(direction), direction) // Walk off "D" upward, end up on "C" facing up
            case Direction.East =>
              (Point(151 - point.x, 150), Direction.West) // Walk off "D" to the right, end up on "A" facing left
            case Direction.South =>
              (Point(point.y + 100, 50), Direction.West) // Walk off "D" downward, end up on "F" facing left
            case Direction.West =>
              (point.move(direction), direction) // Walk off "D" to the left, end up on "E" facing left
          }
        } else if (E.contains(point)) {
          direction match {
            case Direction.North =>
              (Point(point.y + 50, 51), Direction.East) // Walk off "E" upward, end up on "C" facing right
            case Direction.East =>
              (point.move(direction), direction) // Walk off "E" to the right, end up on "D" facing right
            case Direction.South =>
              (point.move(direction), direction) // Walk off "E" downward, end up on "F" facing down
            case Direction.West =>
              (Point(151 - point.x, 51), Direction.East) // Walk off "E" to the left, end up on "B" facing right
          }
        } else if (F.contains(point)) {
          direction match {
            case Direction.North => (point.move(direction), direction) // Walk off "F" upward, end up on "E" facing up
            case Direction.East =>
              (Point(150, point.x - 100), Direction.North) // Walk off "F" to the right, end up on "D" facing up
            case Direction.South =>
              (Point(1, point.y + 100), Direction.South) // Walk off "F" downward, end up on "A" facing down
            case Direction.West =>
              (Point(1, point.x - 100), Direction.South) // Walk off "F" to the left, end up on "B" facing down
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
    def helper(position: Point, instructions: List[Instruction], direction: Direction = Direction.East): Int = {
      instructions match {
        case Nil                       => (1000 * position.x) + (4 * position.y) + direction.toInt
        case Instruction.Left :: tail  => helper(position, tail, direction.left)
        case Instruction.Right :: tail => helper(position, tail, direction.right)
        case Instruction.Move(steps) :: tail =>
          val (newPos, newDirection) = (0 until steps).foldLeft((position, direction)) {
            case ((currentPosition, currentDirection), _) =>
              val next = currentPosition.move(currentDirection)
              grid.get(next) match {
                case Some('#') => (currentPosition, currentDirection)     // Hit a wall, stop
                case Some('.') => (next, currentDirection)                // Open space, continue
                case _         => wrap(currentPosition, currentDirection) // Wrap based on input function
              }
          }
          helper(newPos, tail, newDirection)
      }
    }

    helper(grid.keys.min, initialInstructions)
  }
}
