package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.using
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultWeightedEdge, SimpleWeightedGraph}

import scala.collection.mutable
import scala.io.Source

object Day22 {
  private[year2018] type Region = (Int, Int, Int)
  private implicit class RegionOps(region: Region) {
    // Geological index is not actually used anywhere
    lazy val (_, erosionLevel, riskLevel) = region
  }

  def main(args: Array[String]): Unit = {
    val (grid, target, corner) = using("2018/day22.txt")(parseInput)
    println(s"Part 1: ${part1(grid, target)}")
    println(s"Part 2: ${part2(grid, corner, target)}")
  }

  private[year2018] def parseInput(file: Source): (Map[(Int, Int), Region], (Int, Int), (Int, Int)) = {
    val List(depthLine, targetLine, _*) = file.getLines().toList
    val depth                           = depthLine.split(": ").last.toInt
    val Array(targetX, targetY, _*)     = targetLine.split(": ").last.split(',').map(_.toInt)
    val target                          = (targetX, targetY)
    val cornerX                         = targetX + 100
    val cornerY                         = targetY + 100
    val grid                            = mutable.Map.empty[(Int, Int), (Int, Int, Int)]
    for {
      y <- 0 to cornerY
      x <- 0 to cornerX
    } {
      val geologicalIndex = (x, y) match {
        case (0, 0) | `target` => 0
        case (0, _)          => y * 48271
        case (_, 0)          => x * 16807
        case _               => grid((x - 1, y)).erosionLevel * grid((x, y - 1)).erosionLevel
      }
      val erosionLevel = (geologicalIndex + depth) % 20183
      val riskLevel    = erosionLevel              % 3
      grid.update((x, y), (geologicalIndex, erosionLevel, riskLevel))
    }
    (grid.toMap, target, (cornerX, cornerY))
  }

  private[year2018] def part1(grid: Map[(Int, Int), Region], target: (Int, Int)): Int = {
    val (targetX, targetY) = target
    for {
      x <- 0 to targetX
      y <- 0 to targetY
    } yield grid((x, y)).riskLevel
  }.sum

  private[year2018] def part2(grid: Map[(Int, Int), Region], corner: (Int, Int), target: (Int, Int)): Int = {
    val (rocky, wet, narrow)   = (0, 1, 2)
    val (torch, gear, neither) = (0, 1, 2)
    val validItems = Map(
      rocky  -> (torch, gear),
      wet    -> (gear, neither),
      narrow -> (torch, neither)
    )

    val riskGrid           = grid.view.mapValues(_.riskLevel)
    val (cornerX, cornerY) = corner
    val (targetX, targetY) = target

    val graph = new SimpleWeightedGraph[(Int, Int, Int), DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    for {
      y <- 0 to cornerY
      x <- 0 to cornerX
    } {
      val items = validItems(riskGrid((x, y)))
      val start = (x, y, items._1)
      val end   = (x, y, items._2)
      // Make sure we have both vertices
      graph.addVertex(start)
      graph.addVertex(end)

      // Add edge between start and end
      graph.addEdge(start, end)
      graph.setEdgeWeight(start, end, 7)

      Seq((0, 1), (0, -1), (1, 0), (-1, 0)).foreach {
        case (dx, dy) =>
          val newX = x + dx
          val newY = y + dy
          // Add an edge to swap items
          if (0 <= newX && newX <= cornerX && 0 <= newY && newY <= cornerY) {
            val newItems = validItems(riskGrid((newX, newY)))
            Set(items._1, items._2).intersect(Set(newItems._1, newItems._2)).foreach { item =>
              val from = (x, y, item)
              val to   = (newX, newY, item)

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

    DijkstraShortestPath.findPathBetween(graph, (0, 0, torch), (targetX, targetY, torch)).getWeight.toInt
  }
}
