package io.github.aaronreidsmith.year2025

import java.awt.geom.{Area, Rectangle2D}
import java.awt.{Polygon, Shape}

import io.github.aaronreidsmith.{Point, Solution}

import scala.io.Source

object Day09 extends Solution {
  type I  = (Seq[Rectangle2D], Shape)
  type O1 = Long
  type O2 = Long

  extension (r: Rectangle2D) {
    def size: Long = ((r.getWidth + 1) * (r.getHeight + 1)).toLong
  }

  override def parseInput(file: Source): (Seq[Rectangle2D], Shape) = {
    val points = file
      .getLines()
      .toSeq
      .map { line =>
        val Array(x, y, _*) = line.split(','): @unchecked
        // Note: java.awt.* does x and y backwards from io.github.aaronreidsmith.Point, so this is intentionally
        // flipped to make the rest of the algorithm easy to follow
        Point(y.toInt, x.toInt)
      }
    val rectangles = points
      .combinations(2)
      .map {
        case Seq(p1, p2) =>
          val topLeftY = p1.y.min(p2.y)
          val height   = p1.y.max(p2.y) - topLeftY
          val topLeftX = p1.x.min(p2.x)
          val width    = p1.x.max(p2.x) - topLeftX
          Rectangle2D.Double(topLeftX.toDouble, topLeftY.toDouble, width.toDouble, height.toDouble)
      }
      .toSeq
    val polygon = Polygon()
    points.foreach(point => polygon.addPoint(point.x, point.y))
    // Polygon.contains has an unfortunate optimization that causes it to return false if the rectangle is on the
    // polygon's edge, like it is in the example. Because of this we need to use the more-accurate Area() for our test
    // input
    (rectangles, if (isTest) Area(polygon) else polygon)
  }

  override def part1(input: (Seq[Rectangle2D], Shape)): Long = {
    val (rectangles, _) = input
    rectangles.foldLeft(Long.MinValue)((acc, rectangle) => acc.max(rectangle.size))
  }

  override def part2(input: (Seq[Rectangle2D], Shape)): Long = {
    val (rectangles, shape) = input
    rectangles.foldLeft(Long.MinValue) {
      case (acc, rectangle) if shape.contains(rectangle) => acc.max(rectangle.size)
      case (acc, _)                                      => acc
    }
  }
}
