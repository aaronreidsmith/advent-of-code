package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Direction, Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

@Slow(part2 = true)
object Day06 extends Solution {
  type I  = (Point, Grid[Char])
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): (Point, Grid[Char]) = {
    val grid  = file.toGrid
    val start = grid.collectFirst { case (pos, char) if char == '^' => pos }.get
    (start, grid.updated(start, '.'))
  }

  override def part1(input: (Point, Grid[Char])): Int = {
    val (start, grid) = input
    traverse(start, grid).size
  }

  override def part2(input: (Point, Grid[Char])): Int = {
    val (start, grid)     = input
    val possibleObstacles = traverse(start, grid) - start
    possibleObstacles.foldLeft(0) { (acc, pos) =>
      val updated = grid.updated(pos, '#')
      if (detectLoop(start, updated)) acc + 1 else acc
    }
  }

  private def traverse(start: Point, grid: Grid[Char]): Set[Point] = {
    @tailrec
    def helper(pos: Point, direction: Direction, visited: Set[Point]): Set[Point] = {
      grid.get(pos.move(direction)) match {
        case None => visited
        case Some(char) =>
          val (newPos, newDirection, newVisited) = if (char == '.') {
            val moved = pos.move(direction)
            (moved, direction, visited + moved)
          } else {
            (pos, direction.right, visited)
          }
          helper(newPos, newDirection, newVisited)
      }
    }

    helper(start, Direction.North, Set(start))
  }

  // TODO: Could probably be combined with above
  private def detectLoop(start: Point, grid: Grid[Char]): Boolean = {
    @tailrec
    def helper(pos: Point, direction: Direction, visited: Set[(Point, Direction)]): Boolean = {
      grid.get(pos.move(direction)) match {
        case None => false
        case Some(char) =>
          val (newPos, newDirection, newVisited) = if (char == '.') {
            val moved = pos.move(direction)
            (moved, direction, visited + ((moved, direction)))
          } else {
            val turned = direction.right
            (pos, turned, visited + ((pos, turned)))
          }
          newVisited == visited || helper(newPos, newDirection, newVisited)
      }
    }

    helper(start, Direction.North, Set((start, Direction.North)))
  }
}
