package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{Solution, using}

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends Solution {
  type I  = List[Reindeer]
  type O1 = Int
  type O2 = Int

  private[year2015] case class Reindeer(
      name: String,
      speed: Int,
      moveTime: Int,
      restTime: Int,
      isResting: Boolean = false,
      timeMoving: Int = 0,
      timeResting: Int = 0,
      position: Int = 0,
      points: Int = 0
  )

  private val maxTime = if (isTest) 1000 else 2503

  def run(): Unit = {
    println("Year 2015, Day 14")
    val reindeer = using("2015/day14.txt")(parseInput)
    println(s"Part 1: ${part1(reindeer)}")
    println(s"Part 2: ${part2(reindeer)}")
    println()
  }

  override protected[year2015] def parseInput(file: Source): List[Reindeer] = {
    val reindeerEntry = "^(.*?) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds\\.$".r
    file.getLines().foldLeft(List.empty[Reindeer]) {
      case (acc, reindeerEntry(name, speed, moveTime, restTime)) =>
        Reindeer(name, speed.toInt, moveTime.toInt, restTime.toInt) :: acc
      case _ => throw new IllegalArgumentException
    }
  }

  override protected[year2015] def part1(input: List[Reindeer]): Int = {
    @tailrec
    def helper(reindeer: List[Reindeer], currentTime: Int = 0): Int = {
      if (currentTime >= maxTime) {
        reindeer.map(_.position).max
      } else {
        val moved = reindeer.map { deer =>
          if (deer.isResting) {
            // They just finished resting and start moving, or they continue resting
            if (deer.timeResting >= deer.restTime) {
              deer.copy(isResting = false, timeResting = 0, timeMoving = 1, position = deer.position + deer.speed)
            } else {
              deer.copy(timeResting = deer.timeResting + 1)
            }
          } else {
            // They just finished moving and need to rest, or they continue moving
            if (deer.timeMoving >= deer.moveTime) {
              deer.copy(isResting = true, timeResting = 1, timeMoving = 0)
            } else {
              deer.copy(timeMoving = deer.timeMoving + 1, position = deer.position + deer.speed)
            }
          }
        }
        helper(moved, currentTime + 1)
      }
    }

    helper(input)
  }

  override protected[year2015] def part2(input: List[Reindeer]): Int = {
    @tailrec
    def helper(reindeer: List[Reindeer], currentTime: Int = 0): Int = {
      if (currentTime >= maxTime) {
        reindeer.map(_.points).max
      } else {
        val moved = reindeer.map { deer =>
          if (deer.isResting) {
            // They just finished resting and start moving, or they continue resting
            if (deer.timeResting >= deer.restTime) {
              deer.copy(isResting = false, timeResting = 0, timeMoving = 1, position = deer.position + deer.speed)
            } else {
              deer.copy(timeResting = deer.timeResting + 1)
            }
          } else {
            // They just finished moving and need to rest, or they continue moving
            if (deer.timeMoving >= deer.moveTime) {
              deer.copy(isResting = true, timeResting = 1, timeMoving = 0)
            } else {
              deer.copy(timeMoving = deer.timeMoving + 1, position = deer.position + deer.speed)
            }
          }
        }
        val firstPosition = moved.map(_.position).max
        val withPoints = moved.map {
          case deer if deer.position == firstPosition => deer.copy(points = deer.points + 1)
          case deer                                   => deer
        }
        helper(withPoints, currentTime + 1)
      }
    }

    helper(input)
  }
}
