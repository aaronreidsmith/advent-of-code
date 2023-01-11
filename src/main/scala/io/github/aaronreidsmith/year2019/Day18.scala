package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.implicits.SourceOps
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.collection.mutable
import scala.io.Source

@Slow(part1 = true, part2 = true)
object Day18 extends Solution(2019, 18) {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  private case class Move(from: Point, to: Point, cost: Int, neededKeys: Set[Char])

  private implicit class CharOps(char: Char) {
    def isDoor: Boolean  = 'A' <= char && char <= 'Z'
    def isKey: Boolean   = 'a' <= char && char <= 'z'
    def isRobot: Boolean = char == '@'
    def isWall: Boolean  = char == '#'
  }

  override protected[year2019] def parseInput(file: Source): Grid[Char] = file.toGrid
  override protected[year2019] def part1(input: Grid[Char]): Int        = solution(input)
  override protected[year2019] def part2(input: Grid[Char]): Int = {
    val robot = input.collectFirst { case (point, char) if char.isRobot => point }.get
    val patch = Map(
      robot.up.left    -> '@',
      robot.up         -> '#',
      robot.up.right   -> '@',
      robot.left       -> '#',
      robot            -> '#',
      robot.right      -> '#',
      robot.down.left  -> '@',
      robot.down       -> '#',
      robot.down.right -> '@'
    )
    solution(input ++ patch)
  }

  private def dijkstra(tunnels: Grid[Char], start: Point): Seq[Move] = {
    val cost       = mutable.Map(start -> 0)
    val keysNeeded = mutable.Map(start -> Set.empty[Char])
    val todo       = mutable.PriorityQueue(start)(Ordering.by(cost))

    while (todo.nonEmpty) {
      val point = todo.dequeue()
      point.immediateNeighbors.foreach { neighbor =>
        if (!tunnels(neighbor).isWall && (!cost.contains(neighbor) || cost(point) + 1 < cost(neighbor))) {
          cost(neighbor) = cost(point) + 1
          keysNeeded(neighbor) = if (tunnels(neighbor).isDoor) {
            keysNeeded(point) + tunnels(neighbor).toLower
          } else {
            keysNeeded(point)
          }
          todo.enqueue(neighbor)
        }
      }
    }

    cost.keys.map(point => Move(start, point, cost(point), keysNeeded(point))).toSeq
  }

  private def solution(tunnels: Grid[Char]): Int = {
    val (keys, robots) = tunnels.foldLeft((Set.empty[Point], Set.empty[Point])) {
      case ((currKeys, currRobots), (point, char)) if char.isKey   => (currKeys + point, currRobots)
      case ((currKeys, currRobots), (point, char)) if char.isRobot => (currKeys, currRobots + point)
      case (acc, _)                                                => acc
    }
    val cache = mutable.Map.empty[(Set[Point], Set[Char]), Int].withDefaultValue(Int.MaxValue)

    val routes = (keys ++ robots).foldLeft(Map.empty[(Point, Point), Move]) { (acc, point) =>
      acc ++ dijkstra(tunnels, point).map(move => (move.from, move.to) -> move)
    }

    def helper(
        remainingKeys: Set[Point],
        robots: Set[Point],
        collectedKeys: Set[Char] = Set.empty,
        total: Int = 0,
        result: Int = Int.MaxValue
    ): Int = if (total >= result || total >= cache((robots, collectedKeys))) {
      result
    } else if (remainingKeys.isEmpty) {
      total
    } else {
      cache((robots, collectedKeys)) = total
      val candidates = for {
        from <- robots
        to   <- remainingKeys
      } yield routes.get((from, to))
      candidates.toSeq.flatten
        .filter(_.neededKeys.subsetOf(collectedKeys))
        .sortBy(_.cost)
        .foldLeft(result) {
          case (currResult, Move(from, to, cost, _)) =>
            helper(remainingKeys - to, robots - from + to, collectedKeys + tunnels(to), total + cost, currResult)
          case (currResult, _) => currResult
        }
    }

    helper(keys, robots)
  }

  //  def main(args: Array[String]): Unit = {
  //    val input = using("2019/day18.txt")(_.mkString)
  //    println(s"Part 1: ${Solver.solve(2019, 18, 1, input)}")
  //    println(s"Part 2: ${Solver.solve(2019, 18, 2, input)}")
  //  }
}
