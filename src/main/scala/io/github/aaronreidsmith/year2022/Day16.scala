package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day16 extends Solution {
  type I  = (Map[String, Valve], Map[String, Map[String, Int]], Set[String])
  type O1 = Int
  type O2 = Int

  case class Valve(flow: Int, neighbors: List[String])

  override def parseInput(
      file: Source
  ): (Map[String, Valve], Map[String, Map[String, Int]], Set[String]) = {
    val entry = """^Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)$""".r
    val valves = file.getLines().foldLeft(Map.empty[String, Valve]) {
      case (acc, entry(label, flow, neighbors)) => acc.updated(label, Valve(flow.toInt, neighbors.split(", ").toList))
      case (acc, _)                             => acc
    }
    val distances = valves.keys.map(label => label -> getDistances(valves, label)).toMap
    val toVisit   = valves.collect { case (label, valve) if valve.flow > 0 => label }.toSet

    (valves, distances, toVisit)
  }

  override def part1(
      input: (Map[String, Valve], Map[String, Map[String, Int]], Set[String])
  ): Int = {
    val (valves, distances, toVisit) = input
    def helper(start: String, todo: Set[String] = toVisit, time: Int = 0, pressure: Int = 0): Int = {
      todo.foldLeft(pressure) { (currentMax, next) =>
        val arrival       = time + 1 + distances(start)(next)
        val addedPressure = (30 - arrival) * valves(next).flow
        if (arrival < 30) {
          currentMax.max(helper(next, todo - next, arrival, pressure + addedPressure))
        } else {
          currentMax
        }
      }
    }

    helper("AA")
  }

  override def part2(
      input: (Map[String, Valve], Map[String, Map[String, Int]], Set[String])
  ): Int = {
    val (valves, distances, toVisit) = input
    val cache                        = mutable.Map.empty[(Set[String], Set[String]), Int].withDefaultValue(-1)

    def helper(
        you: String,
        elephant: String,
        todo: Set[String] = toVisit,
        youTime: Int = 4,
        elephantTime: Int = 4,
        pressure: Int = 0
    ): Int = {
      val key = (Set(you, elephant), todo)
      if (cache(key) >= pressure) {
        -1
      } else {
        cache(key) = pressure
        todo.foldLeft(pressure) { (currentMax, next) =>
          val youArrival       = youTime + 1 + distances(you)(next)
          val youAddedPressure = (30 - youArrival) * valves(next).flow
          val youCandidate = if (youArrival < 30) {
            helper(next, elephant, todo - next, youArrival, elephantTime, pressure + youAddedPressure)
          } else {
            0
          }

          val elephantArrival       = elephantTime + 1 + distances(elephant)(next)
          val elephantAddedPressure = (30 - elephantArrival) * valves(next).flow
          val elephantCandidate = if (elephantArrival < 30) {
            helper(you, next, todo - next, youTime, elephantArrival, pressure + elephantAddedPressure)
          } else {
            0
          }

          Seq(currentMax, youCandidate, elephantCandidate).max
        }
      }
    }

    helper("AA", "AA")
  }

  private def getDistances(valves: Map[String, Valve], start: String): Map[String, Int] = {
    val toVisit = mutable.Queue(start)
    val cost    = mutable.Map(start -> 0)

    while (toVisit.nonEmpty) {
      val current = toVisit.dequeue()
      valves(current).neighbors.foreach { next =>
        if (!cost.contains(next)) {
          toVisit.enqueue(next)
          cost(next) = cost(current) + 1
        }
      }
    }

    cost.toMap
  }
}
