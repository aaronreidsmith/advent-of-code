package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day23 extends Solution(2018, 23) {
  type I  = List[NanoBot]
  type O1 = Int
  type O2 = Int

  private[year2018] case class NanoBot(x: Int, y: Int, z: Int, radius: Int) {
    def distanceFrom(that: NanoBot): Int =
      math.abs(this.x - that.x) + math.abs(this.y - that.y) + math.abs(this.z - that.z)

    def distanceFromOrigin: Int = x.abs + y.abs + z.abs

    def inRangeOf(that: NanoBot): Boolean = distanceFrom(that) <= that.radius
  }

  override protected[year2018] def parseInput(file: Source): List[NanoBot] = {
    val nanobot = """^pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)$""".r
    file.getLines().toList.collect {
      case nanobot(x, y, z, r) => NanoBot(x.toInt, y.toInt, z.toInt, r.toInt)
    }
  }

  override protected[year2018] def part1(input: List[NanoBot]): Int = {
    val maxRadius = input.maxBy(_.radius)
    input.count(_.inRangeOf(maxRadius))
  }

  // Adapted from https://www.reddit.com/r/adventofcode/comments/a8s17l/comment/ecdqzdg
  override protected[year2018] def part2(input: List[NanoBot]): Int = {
    val queue = mutable.PriorityQueue.empty[(Int, Int)](Ordering.by(pair => -pair._1))
    input.foreach { bot =>
      queue.enqueue((Seq(0, bot.distanceFromOrigin - bot.radius).max, 1))
      queue.enqueue((bot.distanceFromOrigin + bot.radius + 1, -1))
    }
    var count    = 0
    var maxCount = 0
    var result   = 0
    while (queue.nonEmpty) {
      val (dist, e) = queue.dequeue()
      count += e
      if (count > maxCount) {
        result = dist
        maxCount = count
      }
    }
    result
  }
}
