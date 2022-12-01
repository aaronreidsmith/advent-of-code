package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day02 extends Solution {
  type I  = List[Box]
  type O1 = Int
  type O2 = Int

  private[year2015] case class Box(width: Int, height: Int, length: Int) {
    private lazy val lw           = length * width
    private lazy val wh           = width * height
    private lazy val hl           = height * length
    private lazy val smallestSide = Seq(lw, wh, hl).min
    private lazy val smallestPerimeter =
      Seq(2 * length + 2 * width, 2 * width + 2 * height, 2 * length + 2 * height).min
    private lazy val volume = length * width * height

    lazy val surfaceArea: Int = (2 * lw) + (2 * wh) + (2 * hl) + smallestSide
    lazy val ribbon: Int      = smallestPerimeter + volume
  }

  def run(): Unit = {
    println("Year 2015, Day 2")
    val boxes = using("2015/day02.txt")(parseInput)
    println(s"Part 1: ${part1(boxes)}")
    println(s"Part 2: ${part2(boxes)}")
    println()
  }

  override protected[year2015] def parseInput(file: Source): List[Box] = {
    val box = "^(\\d+)x(\\d+)x(\\d+)$".r

    file.getLines().foldLeft(List.empty[Box]) { (acc, line) =>
      val newBox = line match {
        case box(width, height, length) => Box(width.toInt, height.toInt, length.toInt)
        case _                          => throw new IllegalArgumentException
      }
      newBox :: acc
    }
  }
  override protected[year2015] def part1(boxes: List[Box]): Int = boxes.foldLeft(0)(_ + _.surfaceArea)
  override protected[year2015] def part2(boxes: List[Box]): Int = boxes.foldLeft(0)(_ + _.ribbon)
}
