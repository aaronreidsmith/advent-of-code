package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.extensions.toMutable

import scala.collection.mutable
import scala.io.Source

object Day12 extends Solution {
  type I  = Map[Int, List[Int]]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Map[Int, List[Int]] = {
    val pipeEntry = """^(\d+) <-> (.*)""".r
    file.getLines().foldLeft(Map.empty[Int, List[Int]]) {
      case (acc, pipeEntry(pipe, neighbors)) =>
        val id          = pipe.toInt
        val neighborIds = neighbors.split(", ").map(_.toInt).toList
        acc + (id -> neighborIds)
      case (acc, _) => acc
    }
  }

  override def part1(input: Map[Int, List[Int]]): Int = findConnected(input, 0).size
  override def part2(input: Map[Int, List[Int]]): Int = {
    var groups   = 0
    val programs = input.toMutable
    while (programs.nonEmpty) {
      val group = findConnected(programs.toMap, programs.keys.head)
      group.foreach(programs -= _)
      groups += 1
    }
    groups
  }

  private def findConnected(programs: Map[Int, List[Int]], key: Int): Set[Int] = {
    val connectedPrograms = mutable.Set(key)
    val lookupQueue       = mutable.Queue(key)
    while (lookupQueue.nonEmpty) {
      val newConnected = programs(lookupQueue.dequeue())
      newConnected.foreach { program =>
        if (!connectedPrograms.contains(program)) {
          connectedPrograms.add(program)
          lookupQueue.enqueue(program)
        }
      }
    }
    connectedPrograms.toSet
  }
}
