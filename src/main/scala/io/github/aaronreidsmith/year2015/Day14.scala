package io.github.aaronreidsmith.year2015

import scala.annotation.tailrec
import scala.io.Source

object Day14 {
  private val reindeerEntry =
    "^(.*?) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds\\.$".r

  protected[this] case class Reindeer(
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

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2015/day14.txt")
    val reindeer = input.getLines().foldLeft(List.empty[Reindeer]) {
      case (acc, reindeerEntry(name, speed, moveTime, restTime)) =>
        acc :+ Reindeer(name, speed.toInt, moveTime.toInt, restTime.toInt)
      case _ => throw new IllegalArgumentException
    }
    input.close()

    println(s"Part 1: ${part1(reindeer)}")
    println(s"Part 2: ${part2(reindeer)}")
  }

  @tailrec
  private def part1(reindeer: List[Reindeer], currentTime: Int = 0): Int = if (currentTime >= 2503) {
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
    part1(moved, currentTime + 1)
  }

  @tailrec
  private def part2(reindeer: List[Reindeer], currentTime: Int = 0): Int = if (currentTime >= 2503) {
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
    part2(withPoints, currentTime + 1)
  }
}
