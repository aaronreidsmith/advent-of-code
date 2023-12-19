package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.implicits.toGrid
import io.github.aaronreidsmith.{Direction, Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day16 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  case class Beam(position: Point, direction: Direction) {
    def move: Beam      = this.copy(position = position.move(direction))
    def turnLeft: Beam  = this.copy(direction = direction.left)
    def turnRight: Beam = this.copy(direction = direction.right)
  }

  object Beam {
    def default: Beam = Beam(Point.zero, Direction.East)
  }

  override def parseInput(file: Source): Grid[Char] = file.toGrid

  override def part1(input: Grid[Char]): Int = energize(input, List(Beam.default))

  override def part2(input: Grid[Char]): Int = {
    val Point(maxX, maxY) = input.keys.max
    val (topEdge, bottomEdge) =
      (0 to maxY).map(i => (Beam(Point(0, i), Direction.South), Beam(Point(maxX, i), Direction.North))).unzip
    val (leftEdge, rightEdge) =
      (0 to maxX).map(i => (Beam(Point(i, 0), Direction.East), Beam(Point(i, maxY), Direction.West))).unzip
    (topEdge ++ leftEdge ++ rightEdge ++ bottomEdge).foldLeft(Int.MinValue) { (acc, beam) =>
      acc.max(energize(input, List(beam)))
    }
  }

  @tailrec
  private def energize(grid: Grid[Char], beams: List[Beam], seen: Set[Beam] = Set.empty): Int = beams match {
    case Nil => seen.map(_.position).size
    case head :: tail =>
      if (seen.contains(head) || !grid.contains(head.position)) {
        energize(grid, tail, seen)
      } else {
        val moved = grid(head.position) match {
          case '.' => head.move :: tail
          case '\\' =>
            head.direction match {
              case Direction.East | Direction.West   => head.turnRight.move :: tail
              case Direction.North | Direction.South => head.turnLeft.move :: tail
            }
          case '/' =>
            head.direction match {
              case Direction.East | Direction.West   => head.turnLeft.move :: tail
              case Direction.North | Direction.South => head.turnRight.move :: tail
            }
          case '|' =>
            head.direction match {
              case Direction.East | Direction.West => head.turnLeft.move :: head.turnRight.move :: tail
              case _                               => head.move :: tail
            }
          case '-' =>
            head.direction match {
              case Direction.North | Direction.South => head.turnLeft.move :: head.turnRight.move :: tail
              case _                                 => head.move :: tail
            }
          case _ => throw new IllegalArgumentException()
        }
        energize(grid, moved, seen + head)
      }
  }
}
