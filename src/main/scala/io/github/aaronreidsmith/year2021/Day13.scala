package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

object Day13 extends Solution {
  type I  = (Set[Point], List[Fold])
  type O1 = Int
  type O2 = String

  enum Axis {
    case Vertical, Horizontal
  }

  case class Fold(axis: Axis, value: Int) {
    def mirror(point: Point): Point = axis match {
      case Axis.Vertical   => point.copy(x = value - (point.x - value).abs)
      case Axis.Horizontal => point.copy(y = value - (point.y - value).abs)
    }
  }

  override def parseInput(file: Source): (Set[Point], List[Fold]) = {
    val verticalFold   = """^fold along x=(\d+)$""".r
    val horizontalFold = """^fold along y=(\d+)$""".r

    val Array(rawPoints, rawFolds, _*) = file.mkString.trim.split("\n\n"): @unchecked
    val grid = rawPoints
      .split('\n')
      .map { line =>
        val Array(x, y, _*) = line.split(','): @unchecked
        Point(x.toInt, y.toInt)
      }
      .toSet
    val folds = rawFolds
      .split('\n')
      .collect {
        case verticalFold(value)   => Fold(Axis.Vertical, value.toInt)
        case horizontalFold(value) => Fold(Axis.Horizontal, value.toInt)
      }
      .toList
    (grid, folds)
  }

  override def part1(input: (Set[Point], List[Fold])): Int = {
    val (grid, folds) = input
    grid.map(folds.head.mirror).size
  }

  override def part2(input: (Set[Point], List[Fold])): String = {
    val (grid, folds) = input
    val folded        = folds.foldLeft(grid)((acc, fold) => acc.map(fold.mirror))
    val (xs, ys)      = folded.unzip(point => (point.x, point.y))
    val output        = new StringBuilder("\n")
    (0 to ys.max).foreach { y =>
      (0 to xs.max).foreach { x =>
        val char = if (folded.contains(Point(x, y))) '#' else ' '
        output.append(char)
      }
      output.append('\n')
    }
    output.result()
  }
}
