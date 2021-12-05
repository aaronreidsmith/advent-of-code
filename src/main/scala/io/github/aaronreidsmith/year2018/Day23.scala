package io.github.aaronreidsmith.year2018

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day23 {
  private val nanobot = "^pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)$".r

  private case class NanoBot(x: Int, y: Int, z: Int, radius: Int) {
    def distanceFrom(that: NanoBot): Int =
      math.abs(this.x - that.x) + math.abs(this.y - that.y) + math.abs(this.z - that.z)
    def distanceFromOrigin: Int           = x.abs + y.abs + z.abs
    def inRangeOf(that: NanoBot): Boolean = distanceFrom(that) <= that.radius
  }

  def main(args: Array[String]): Unit = {
    val nanobots = Using.resource(Source.fromResource("2018/day23.txt")) { file =>
      file.getLines().toSeq.map {
        case nanobot(x, y, z, r) => NanoBot(x.toInt, y.toInt, z.toInt, r.toInt)
      }
    }

    val maxRadius = nanobots.maxBy(_.radius)
    val part1     = nanobots.count(_.inRangeOf(maxRadius))
    println(s"Part 1: $part1")

    // Adapted from https://www.reddit.com/r/adventofcode/comments/a8s17l/comment/ecdqzdg
    val part2 = {
      val q = mutable.PriorityQueue.empty[(Int, Int)](Ordering.by(pair => -pair._1))
      nanobots.foreach { bot =>
        q.enqueue((Seq(0, bot.distanceFromOrigin - bot.radius).max, 1))
        q.enqueue((bot.distanceFromOrigin + bot.radius + 1, -1))
      }
      var count    = 0
      var maxCount = 0
      var result   = 0
      while (q.nonEmpty) {
        val (dist, e) = q.dequeue()
        count += e
        if (count > maxCount) {
          result = dist
          maxCount = count
        }
      }
      result
    }
    println(s"Part 2: $part2")
  }
}
