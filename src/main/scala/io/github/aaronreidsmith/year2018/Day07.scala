package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.implicits.MapOps

import scala.collection.mutable
import scala.io.Source

// TODO: Adapted from Raku solution
object Day07 extends Solution(2018, 7) {
  type I  = Map[Char, Set[Char]]
  type O1 = String
  type O2 = Int

  override protected[year2018] def parseInput(file: Source): Map[Char, Set[Char]] = {
    val regex = "^Step ([A-Z]) must be finished before step ([A-Z]) can begin.$".r

    val fromFile = file.getLines().foldLeft(Map.empty[Char, Set[Char]]) {
      case (acc, regex(keyGroup, valueGroup)) =>
        // Turns strings to chars
        val key      = keyGroup.head
        val value    = valueGroup.head
        val existing = acc.getOrElse(key, Set.empty)
        acc.updated(key, existing + value)
      case (acc, _) => acc
    }

    // Have to add a pointer from the last item to nothing
    val last = fromFile.values.minBy(_.size).head
    fromFile + (last -> Set.empty)
  }

  override protected[year2018] def part1(input: Map[Char, Set[Char]]): String = solution(input, 1)._1
  override protected[year2018] def part2(input: Map[Char, Set[Char]]): Int    = solution(input, 5)._2

  private def solution(dependencies: Map[Char, Set[Char]], numWorkers: Int): (String, Int) = {
    var duration    = 0
    val mutableDeps = dependencies.toMutable
    val work        = mutable.Map.empty[Char, Int]
    val order       = new StringBuilder
    while (mutableDeps.nonEmpty) {
      val next = mutableDeps.keys.toSet.diff(mutableDeps.values.reduceLeft(_ ++ _)).diff(work.keys.toSet).toSeq.sorted

      val free = numWorkers - work.size
      next.take(free).foreach { char =>
        work.update(char, 60 + (char.toInt - 64))
      }

      val timestep = work.values.min
      val done = work
        .flatMap {
          case (key, value) =>
            val newValue = value - timestep
            if (newValue == 0) {
              work.remove(key)
              mutableDeps.remove(key)
              Some(key)
            } else {
              work.update(key, newValue)
              None
            }
        }
        .toSeq
        .sorted

      duration += timestep
      order ++= done
    }
    (order.mkString, duration)
  }
}
