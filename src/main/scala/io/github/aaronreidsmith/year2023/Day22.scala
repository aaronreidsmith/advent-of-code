package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

object Day22 extends Solution {
  type I  = Seq[Brick]
  type O1 = Int
  type O2 = Int

  case class Point3D(x: Int, y: Int, z: Int) {
    def drop(dz: Int): Point3D = this.copy(z = this.z - dz)
  }
  case class Brick(start: Point3D, end: Point3D) {
    def points2D: Seq[Point] = {
      for {
        x <- start.x to end.x
        y <- start.y to end.y
      } yield Point(x, y)
    }

    def drop(tallest: Map[Point, Int]): Brick = {
      val peak = this.points2D.foldLeft(0)((acc, point) => acc.max(tallest.getOrElse(point, 0)))
      val dz   = this.start.z - peak - 1
      this.copy(start = this.start.drop(dz), end = this.end.drop(dz))
    }
  }

  override def parseInput(file: Source): Seq[Brick] = file
    .getLines()
    .toSeq
    .map { line =>
      val Array(startRaw, endRaw, _*)       = line.split('~'): @unchecked
      val Array(startX, startY, startZ, _*) = startRaw.split(',').map(_.toInt): @unchecked
      val Array(endX, endY, endZ, _*)       = endRaw.split(',').map(_.toInt): @unchecked
      Brick(Point3D(startX, startY, startZ), Point3D(endX, endY, endZ))
    }
    .sortBy(_.start.z)

  override def part1(input: Seq[Brick]): Int = solution(input)._1
  override def part2(input: Seq[Brick]): Int = solution(input)._2

  private var part1Solution = 0
  private var part2Solution = 0
  private var solved        = false
  private def solution(bricks: Seq[Brick]): (Int, Int) = {
    if (!solved) {
      val (_, fallen) = drop(bricks)
      fallen.indices.foreach { i =>
        val removed    = fallen.take(i) ++ fallen.drop(i + 1)
        val (falls, _) = drop(removed)
        if (falls == 0) {
          part1Solution += 1
        } else {
          part2Solution += falls
        }
      }
      solved = true
    }
    (part1Solution, part2Solution)
  }

  private def drop(bricks: Seq[Brick]): (Int, Seq[Brick]) = {
    val (_, falls, newBricks) = bricks.foldLeft((Map.empty[Point, Int], 0, Vector.empty[Brick])) {
      case ((tallest, fallsAcc, towerAcc), brick) =>
        val newBrick   = brick.drop(tallest)
        val newTallest = brick.points2D.foldLeft(tallest)((acc, point) => acc.updated(point, newBrick.end.z))
        val newFalls   = if (newBrick.start.z != brick.start.z) fallsAcc + 1 else fallsAcc
        val newTower   = towerAcc :+ newBrick
        (newTallest, newFalls, newTower)
    }
    (falls, newBricks)
  }
}
