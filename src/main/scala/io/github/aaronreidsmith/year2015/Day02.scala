package io.github.aaronreidsmith.year2015

import scala.io.Source

object Day02 {
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

  private val box = "^(\\d+)x(\\d+)x(\\d+)$".r

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2015/day02.txt")
    val boxes = input.getLines().foldLeft(List.empty[Box]) { (acc, line) =>
      val newBox = line match {
        case box(width, height, length) => Box(width.toInt, height.toInt, length.toInt)
        case _                          => throw new IllegalArgumentException
      }
      newBox :: acc
    }
    println(s"Part 1: ${part1(boxes)}")
    println(s"Part 2: ${part2(boxes)}")
  }

  private[year2015] def part1(boxes: List[Box]): Int = boxes.foldLeft(0)(_ + _.surfaceArea)
  private[year2015] def part2(boxes: List[Box]): Int = boxes.foldLeft(0)(_ + _.ribbon)
}
