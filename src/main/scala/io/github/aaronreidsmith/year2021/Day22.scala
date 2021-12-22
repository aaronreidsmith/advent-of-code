package io.github.aaronreidsmith.year2021

import scala.io.Source
import scala.util.Using

object Day22 {
  private val step = """^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)""".r

  def main(args: Array[String]): Unit = {
    val steps = Using.resource(Source.fromResource("2021/day22.txt")) { file =>
      file.getLines().toList.map {
        case step(state, x0, x1, y0, y1, z0, z1) =>
          (state, x0.toLong, x1.toLong, y0.toLong, y1.toLong, z0.toLong, z1.toLong)
        case _ => throw new IllegalArgumentException
      }
    }

    println(s"Part 1: ${part1(steps)}")
    println(s"Part 2: ${part2(steps)}")
  }

  private def part1(steps: List[(String, Long, Long, Long, Long, Long, Long)]): Long = steps
    .foldLeft(Set.empty[(Long, Long, Long)]) {
      case (acc, (state, x0, x1, y0, y1, z0, z1)) =>
        val updated = for {
          x <- x0.min(x1) to x0.max(x1) if x >= -50 && x <= 50
          y <- y0.min(y1) to y0.max(y1) if y >= -50 && y <= 50
          z <- z0.min(z1) to z0.max(z1) if z >= -50 && z <= 50
        } yield (x, y, z)

        if (state == "on") acc ++ updated else acc -- updated
      case (acc, _) => acc
    }
    .size

  // Adapted from https://www.reddit.com/r/adventofcode/comments/rlxhmg/comment/hplp672
  private def part2(steps: List[(String, Long, Long, Long, Long, Long, Long)]): Long = {
    def count(cubes: List[(String, Long, Long, Long, Long, Long, Long)]): Long = cubes match {
      case Nil => 0
      case (state, x0, x1, y0, y1, z0, z1) :: tail =>
        if (state == "off") {
          count(tail)
        } else {
          val minus = tail.foldLeft(Set.empty[(String, Long, Long, Long, Long, Long, Long)]) {
            case (acc, (_, u0, u1, v0, v1, w0, w1)) =>
              val intersection = intersect(x0, x1, y0, y1, z0, z1, u0, u1, v0, v1, w0, w1).map(Set(_)).getOrElse(Set())
              acc ++ intersection
            case (acc, _) => acc
          }
          volume(x0, x1, y0, y1, z0, z1) + count(tail) - count(minus.toList)
        }
    }

    def intersect(
        x0: Long,
        x1: Long,
        y0: Long,
        y1: Long,
        z0: Long,
        z1: Long,
        u0: Long,
        u1: Long,
        v0: Long,
        v1: Long,
        w0: Long,
        w1: Long
    ): Option[(String, Long, Long, Long, Long, Long, Long)] = {
      val actualX0 = if (x0 > u0) x0 else u0
      val actualX1 = if (x1 < u1) x1 else u1
      val actualY0 = if (y0 > v0) y0 else v0
      val actualY1 = if (y1 < v1) y1 else v1
      val actualZ0 = if (z0 > w0) z0 else w0
      val actualZ1 = if (z1 < w1) z1 else w1
      if (actualX0 <= actualX1 && actualY0 <= actualY1 && actualZ0 <= actualZ1) {
        Some(("", actualX0, actualX1, actualY0, actualY1, actualZ0, actualZ1))
      } else {
        None
      }
    }

    def volume(x0: Long, x1: Long, y0: Long, y1: Long, z0: Long, z1: Long): Long =
      (x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1)

    count(steps)
  }
}
