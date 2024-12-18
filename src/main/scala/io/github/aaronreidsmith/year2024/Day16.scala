package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Direction, Grid, Point, Solution}
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultWeightedEdge}

import scala.io.Source

object Day16 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Grid[Char] = file.toGrid

  override def part1(input: Grid[Char]): Int = {
    val points = input.collect { case (p, c) if c != '#' => p }.toSet
    val start  = input.collectFirst { case (p, c) if c == 'S' => p }.get
    val end    = input.collectFirst { case (p, c) if c == 'E' => p }.get

    val graph = new DefaultDirectedWeightedGraph[(Point, Direction), DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    for {
      point     <- points
      direction <- Direction.values
    } {
      graph.addVertex((point, direction))

      graph.addVertex((point.move(direction), direction))
      graph.addEdge((point, direction), (point.move(direction), direction))
      graph.setEdgeWeight((point, direction), (point.move(direction), direction), 1)

      graph.addVertex((point, direction.left))
      graph.addEdge((point, direction), (point, direction.left))
      graph.setEdgeWeight((point, direction), (point, direction.left), 1000)

      graph.addVertex((point, direction.right))
      graph.addEdge((point, direction), (point, direction.right))
      graph.setEdgeWeight((point, direction), (point, direction.right), 1000)
    }

    Seq(Direction.East, Direction.North).foldLeft(Int.MaxValue) { (acc, endDirection) =>
      acc.min(DijkstraShortestPath.findPathBetween(graph, (start, Direction.East), (end, endDirection)).getWeight.toInt)
    }
  }
}
