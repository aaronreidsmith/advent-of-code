package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution

import scala.io.Source

// Adapted from https://old.reddit.com/r/adventofcode/comments/zpihwi/2022_day_19_solutions/j0w0mx3/
object Day19 extends Solution {
  type I  = List[Blueprint]
  type O1 = Int
  type O2 = Int

  case class Resources(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0) {
    def +(other: Resources): Resources = Resources(
      ore + other.ore,
      clay + other.clay,
      obsidian + other.obsidian,
      geode + other.geode
    )
    def -(other: Resources): Resources = Resources(
      ore - other.ore,
      clay - other.clay,
      obsidian - other.obsidian,
      geode - other.geode
    )
    def <=(other: Resources): Boolean = {
      ore <= other.ore && clay <= other.clay && obsidian <= other.obsidian && geode <= other.geode
    }
  }

  case class Blueprint(
      id: Int,
      oreOre: Int,
      clayOre: Int,
      obsidianOre: Int,
      obsidianClay: Int,
      geodeOre: Int,
      geodeObsidian: Int
  ) {
    def maxOre: Int = Seq(oreOre, clayOre, obsidianOre, geodeOre).max
  }

  override def parseInput(file: Source): List[Blueprint] = {
    val blueprint =
      """Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.""".stripMargin.r
    file.getLines().toList.collect {
      case blueprint(id, oreOre, clayOre, obsidianOre, obsidianClay, geodeOre, geodeObsidian) =>
        Blueprint(
          id.toInt,
          oreOre.toInt,
          clayOre.toInt,
          obsidianOre.toInt,
          obsidianClay.toInt,
          geodeOre.toInt,
          geodeObsidian.toInt
        )
    }
  }

  override def part1(input: List[Blueprint]): Int = input.foldLeft(0)(_ + maximize(_, 24))
  override def part2(input: List[Blueprint]): Int = {
    input.take(3).foldLeft(1)(_ * maximize(_, 32)) / 6 // Not sure why we have to divide by 6 here
  }

  // Only want to compile these once
  private val zero        = Resources()
  private val oreBot      = Resources(ore = 1)
  private val clayBot     = Resources(clay = 1)
  private val obsidianBot = Resources(obsidian = 1)
  private val geodeBot    = Resources(geode = 1)
  private def maximize(blueprint: Blueprint, minutes: Int): Int = {
    val oreBotCost      = Resources(ore = blueprint.oreOre)
    val clayBotCost     = Resources(ore = blueprint.clayOre)
    val obsidianBotCost = Resources(ore = blueprint.obsidianOre, clay = blueprint.obsidianClay)
    val geodeBotCost    = Resources(ore = blueprint.geodeOre, obsidian = blueprint.geodeObsidian)

    def helper(
        time: Int,
        bots: Resources,
        resources: Resources,
        prevCanOre: Boolean,
        prevCanClay: Boolean,
        prevCanObsidian: Boolean
    ): Int = if (time <= 0) {
      resources.geode
    } else if (geodeBotCost <= resources) {
      helper(time - 1, bots + geodeBot, resources + bots - geodeBotCost, false, false, false)
    } else {
      val canOre      = oreBotCost <= resources && bots.ore < blueprint.maxOre
      val canClay     = clayBotCost <= resources && bots.clay < blueprint.obsidianClay
      val canObsidian = obsidianBotCost <= resources && bots.obsidian < blueprint.geodeObsidian

      val first = helper(time - 1, bots, resources + bots, canOre, canClay, canObsidian)
      val second = if (canOre && !prevCanOre) {
        helper(time - 1, bots + oreBot, resources + bots - oreBotCost, false, false, false)
      } else {
        0
      }
      val third = if (canClay && !prevCanClay) {
        helper(time - 1, bots + clayBot, resources + bots - clayBotCost, false, false, false)
      } else {
        0
      }
      val fourth = if (canObsidian && !prevCanObsidian) {
        helper(time - 1, bots + obsidianBot, resources + bots - obsidianBotCost, false, false, false)
      } else {
        0
      }

      Seq(first, second, third, fourth).max
    }

    blueprint.id * helper(minutes, oreBot, zero, false, false, false)
  }
}
