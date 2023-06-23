package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{Point, Solution}
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultUndirectedWeightedGraph, DefaultWeightedEdge}

import scala.collection.mutable
import scala.io.Source

object Day22 extends Solution {
  type I  = (Map[Point, Region], Point)
  type O1 = Int
  type O2 = Int

  case class Region(geologicalIndex: Int, erosionLevel: Int, riskLevel: Int)

  override def parseInput(file: Source): (Map[Point, Region], Point) = {
    val List(depthLine, targetLine, _*) = file.getLines().toList: @unchecked
    val depth                           = depthLine.split(": ").last.toInt
    val Array(targetX, targetY, _*)     = targetLine.split(": ").last.split(',').map(_.toInt): @unchecked
    val target                          = Point(targetX, targetY)
    val grid                            = mutable.Map.empty[Point, Region]
    for {
      x <- 0 to targetX + 100
      y <- 0 to targetY + 100
      point = Point(x, y)
    } {
      val geologicalIndex = point match {
        case Point(0, 0) | `target` => 0
        case Point(0, _)            => y * 48271
        case Point(_, 0)            => x * 16807
        case _                      => grid(point.left).erosionLevel * grid(point.up).erosionLevel
      }
      val erosionLevel = (geologicalIndex + depth) % 20183
      val riskLevel    = erosionLevel              % 3
      grid.update(point, Region(geologicalIndex, erosionLevel, riskLevel))
    }
    (grid.toMap, target)
  }

  override def part1(input: (Map[Point, Region], Point)): Int = {
    val (grid, target) = input
    for {
      x <- 0 to target.x
      y <- 0 to target.y
    } yield grid(Point(x, y)).riskLevel
  }.sum

  override def part2(input: (Map[Point, Region], Point)): Int = {
    val (grid, target)         = input
    val (rocky, wet, narrow)   = (0, 1, 2)
    val (torch, gear, neither) = (0, 1, 2)
    val validItems = Map(
      rocky  -> (torch, gear),
      wet    -> (gear, neither),
      narrow -> (torch, neither)
    )

    val riskGrid = grid.view.mapValues(_.riskLevel).toMap

    val graph = new DefaultUndirectedWeightedGraph[(Point, Int), DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    grid.keys.foreach { point =>
      val items = validItems(riskGrid(point))
      val start = (point, items._1)
      val end   = (point, items._2)
      // Make sure we have both vertices
      graph.addVertex(start)
      graph.addVertex(end)

      // Add edge between start and end
      graph.addEdge(start, end)
      graph.setEdgeWeight(start, end, 7)

      point.immediateNeighbors.foreach { neighbor =>
        if (grid.contains(neighbor)) {
          val newItems = validItems(riskGrid(neighbor))
          Set(items._1, items._2).intersect(Set(newItems._1, newItems._2)).foreach { item =>
            val from = (point, item)
            val to   = (neighbor, item)

            // Make sure we have both vertices
            graph.addVertex(from)
            graph.addVertex(to)

            // Add edge between them
            graph.addEdge(from, to)
            graph.setEdgeWeight(from, to, 1)
          }
        }
      }
    }

    DijkstraShortestPath.findPathBetween(graph, (Point.zero, torch), (target, torch)).getWeight.toInt
  }
}
