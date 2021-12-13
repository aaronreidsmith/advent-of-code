package io.github.aaronreidsmith.year2021

import scala.io.Source
import scala.util.Using

object Day13 {
  private val verticalFold   = "^fold along x=(\\d+)$".r
  private val horizontalFold = "^fold along y=(\\d+)$".r

  private sealed trait Axis
  private case object Vertical   extends Axis
  private case object Horizontal extends Axis

  private case class Fold(axis: Axis, value: Int) {
    def mirror(point: (Int, Int)): (Int, Int) = {
      val (x, y) = point
      axis match {
        case Vertical =>
          val diff = (x - value).abs
          (value - diff, y)
        case Horizontal =>
          val diff = (y - value).abs
          (x, value - diff)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val input                          = Using.resource(Source.fromResource("2021/day13.txt"))(_.mkString)
    val Array(rawPoints, rawFolds, _*) = input.split("\n\n")
    val grid = rawPoints
      .split('\n')
      .map { line =>
        val Array(x, y, _*) = line.split(',')
        (x.toInt, y.toInt)
      }
      .toSet
    val folds = rawFolds
      .split('\n')
      .map {
        case verticalFold(value)   => Fold(Vertical, value.toInt)
        case horizontalFold(value) => Fold(Horizontal, value.toInt)
      }
      .toList
    println(s"Part 1: ${part1(grid, folds.head)}")
    println(s"Part 2:\n${part2(grid, folds)}")
  }

  def part1(grid: Set[(Int, Int)], fold: Fold): Int = grid.map(fold.mirror).size

  def part2(grid: Set[(Int, Int)], folds: List[Fold]): String = {
    val folded = folds.foldLeft(grid)((acc, fold) => acc.map(fold.mirror))
    val maxX   = folded.map(_._1).max
    val maxY   = folded.map(_._2).max
    (0 to maxY)
      .foldLeft(Vector.empty[String]) { (acc, y) =>
        acc :+ (0 to maxX)
          .foldLeft(Vector.empty[Char]) { (innerAcc, x) =>
            val char = if (folded.contains((x, y))) '#' else ' '
            innerAcc :+ char
          }
          .mkString
      }
      .mkString("\n")
  }
}
