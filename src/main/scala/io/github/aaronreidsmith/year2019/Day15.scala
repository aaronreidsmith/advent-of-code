package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{Point, Solution}

import scala.collection.mutable
import scala.io.Source

object Day15 extends Solution {
  type I  = IntCode
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): IntCode = IntCode(file)

  override def part1(input: IntCode): Int = traverse(input) match {
    case (first, Some((target, _))) => first(target)
    case _                          => -1
  }

  override def part2(input: IntCode): Int = traverse(input) match {
    case (_, Some((_, computer))) => traverse(computer)._1.values.max
    case _                        => -1
  }

  private def traverse(initial: IntCode): (Map[Point, Int], Option[(Point, IntCode)]) = {
    val neighbors = Seq(
      (Point(0, -1), 1),
      (Point(0, 1), 2),
      (Point(-1, 0), 3),
      (Point(1, 0), 4)
    )

    def move(point: Point, computer: IntCode, delta: Point, command: Int): (Point, IntCode, Long) = {
      val nextPoint    = point + delta
      val nextComputer = computer.withInput(command).nextOutput
      val status = nextComputer.result match {
        case IntCode.Output(s) => s
        case _                 => 0L
      }
      (nextPoint, nextComputer, status)
    }

    val cost   = mutable.Map(Point.zero -> 0)
    val todo   = mutable.PriorityQueue((Point.zero, initial))(Ordering.by { case (point, _) => cost(point) })
    var target = Option.empty[(Point, IntCode)]

    while (todo.nonEmpty) {
      val (point, computer) = todo.dequeue()
      neighbors.foreach { neighbor =>
        val (delta, command)                  = neighbor
        val (nextPoint, nextComputer, status) = move(point, computer, delta, command)
        if (status > 0 && (!cost.contains(nextPoint) || cost(point) + 1 < cost(nextPoint))) {
          cost(nextPoint) = cost(point) + 1
          todo.enqueue((nextPoint, nextComputer))
          if (status == 2) {
            target = Some((nextPoint, nextComputer))
          }
        }
      }
    }

    (cost.toMap, target)
  }
}
