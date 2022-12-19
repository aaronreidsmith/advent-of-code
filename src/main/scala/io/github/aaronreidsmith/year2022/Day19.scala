package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import scala.io.Source
import scala.collection.parallel.CollectionConverters._

// Adapted from https://old.reddit.com/r/adventofcode/comments/zpihwi/2022_day_19_solutions/j0uis7k/
// Don't know how this works and don't really care...
// TODO: Make more idiomatic
// TODO: Clean up variable names
@Slow(part2 = true)
object Day19 extends Solution(2022, 19) {
  type I  = List[Array[Int]]
  type O1 = Int
  type O2 = Int

  override protected[year2022] def parseInput(file: Source): List[Array[Int]] = {
    val blueprint =
      """Blueprint \d+: Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.""".r
    file.getLines().toList.collect {
      case blueprint(oreOreCost, clayOreCost, obsidianOreCost, obsidianClayCost, geodeOreCost, geodeObsidianCost) =>
        Array(
          oreOreCost.toInt,
          clayOreCost.toInt,
          obsidianOreCost.toInt,
          obsidianClayCost.toInt,
          geodeOreCost.toInt,
          geodeObsidianCost.toInt
        )
    }
  }

  override protected[year2022] def part1(input: List[Array[Int]]): Int = input.zipWithIndex.par.foldLeft(0) {
    case (acc, (blueprint, i)) => acc + ((i + 1) * maxSim(Array(1, 0, 0, 0), Array(1, 0, 0, 0), blueprint, 1, 24))
    case (acc, _)              => acc
  }

  override protected[year2022] def part2(input: List[Array[Int]]): Int = {
    input.take(3).par.foldLeft(1) { (acc, blueprint) =>
      acc * maxSim(Array(1, 0, 0, 0), Array(1, 0, 0, 0), blueprint, 1, 32)
    }
  }

  private def nextState(
      rs: Array[Int],
      cs: Array[Int],
      p: Int,
      r1: Int,
      c1: Int,
      r2: Int,
      c2: Int
  ): (Array[Int], Array[Int], Int) = {
    var w = math.max((c1 - cs(r1) + rs(r1) - 1) / rs(r1), (c2 - cs(r2) + rs(r2) - 1) / rs(r2))
    if (w < 0) {
      w = 0
    }
    w += 1

    val ncs = Array(cs(0) + rs(0) * w, cs(1) + rs(1) * w, cs(2) + rs(2) * w, cs(3) + rs(3) * w)
    ncs(r1) -= c1
    ncs(r2) -= c2

    val nrs = Array(rs(0), rs(1), rs(2), rs(3))
    nrs(p) += 1

    (nrs, ncs, w)
  }

  private def maxSim(rs: Array[Int], cs: Array[Int], blueprint: Array[Int], r: Int, maxr: Int): Int = {
    var max = 0
    var w   = 0
    var nrs = Array.empty[Int]
    var ncs = Array.empty[Int]
    if (r > maxr) {
      return 0
    }
    if (r == maxr) {
      return cs(3)
    }
    // check if we can build a geode robot first. No max.
    if (rs(2) > 0) {
      val next = nextState(rs, cs, 3, 0, blueprint(4), 2, blueprint(5))
      nrs = next._1
      ncs = next._2
      w = next._3
      var tmp = cs(3) + rs(3) * (maxr - r)
      if (r + w <= maxr) {
        tmp = maxSim(nrs, ncs, blueprint, r + w, maxr)
      }
      max = math.max(max, tmp)
      if (w == 1) {
        return max
      }
    }
    // build ore robot next, up to 6 max (may need to change based on input)
    if (rs(0) < 6) {
      val next = nextState(rs, cs, 0, 0, blueprint(0), 0, 0)
      nrs = next._1
      ncs = next._2
      w = next._3
      max = math.max(max, maxSim(nrs, ncs, blueprint, r + w, maxr))
    }
    // same for a clay robot, up to 10 max (may need to change based on input)
    if (rs(1) < 10) {
      val next = nextState(rs, cs, 1, 0, blueprint(1), 0, 0)
      nrs = next._1
      ncs = next._2
      w = next._3
      max = math.max(max, maxSim(nrs, ncs, blueprint, r + w, maxr))
    }
    // same for an obsidian robot, up to 10 max (may need to change based on input)
    if (rs(1) > 0 && rs(2) < 10) {
      val next = nextState(rs, cs, 2, 0, blueprint(2), 1, blueprint(3))
      nrs = next._1
      ncs = next._2
      w = next._3
      max = math.max(max, maxSim(nrs, ncs, blueprint, r + w, maxr));
    }

    max
  }
}
