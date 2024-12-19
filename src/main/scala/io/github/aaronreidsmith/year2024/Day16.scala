package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Direction, Grid, Point, Solution}
import org.jgrapht.GraphPath
import org.jgrapht.alg.shortestpath.{AllDirectedPaths, DijkstraShortestPath, PathValidator}
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultWeightedEdge}

import scala.io.Source
import scala.jdk.CollectionConverters.*

object Day16 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Grid[Char] = file.toGrid

  override def part1(input: Grid[Char]): Int = {
    bestPathWeight.tupled(buildGraph(input))
  }

  // Takes unreasonably long on actual endpoint
  override def part2(input: Grid[Char]): Int = {
    val (start, end, graph) = buildGraph(input)
    val best                = bestPathWeight(start, end, graph)
    val allPaths = new AllDirectedPaths(
      graph,
      (partialPath: GraphPath[(Point, Direction), DefaultWeightedEdge], _: DefaultWeightedEdge) => {
        partialPath.getWeight.toInt < best
      }
    )
    Seq(Direction.North, Direction.East)
      .foldLeft(Set.empty[Point]) { (acc, direction) =>
        acc ++ allPaths
          .getAllPaths((start, Direction.East), (end, direction), true, null)
          .asScala
          .foldLeft(Set.empty[Point]) { (inner, path) =>
            inner ++ path.getVertexList.asScala.map(_._1).toSet
          }
      }
      .size
  }

  private def buildGraph(
      grid: Grid[Char]
  ): (Point, Point, DefaultDirectedWeightedGraph[(Point, Direction), DefaultWeightedEdge]) = {
    val points = grid.collect { case (p, c) if c != '#' => p }.toSet
    val start  = grid.collectFirst { case (p, c) if c == 'S' => p }.get
    val end    = grid.collectFirst { case (p, c) if c == 'E' => p }.get

    val graph = DefaultDirectedWeightedGraph[(Point, Direction), DefaultWeightedEdge](classOf[DefaultWeightedEdge])
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
    (start, end, graph)
  }

  private def bestPathWeight(
      start: Point,
      end: Point,
      graph: DefaultDirectedWeightedGraph[(Point, Direction), DefaultWeightedEdge]
  ): Int = {
    Seq(Direction.East, Direction.North).foldLeft(Int.MaxValue) { (acc, endDirection) =>
      acc.min(DijkstraShortestPath.findPathBetween(graph, (start, Direction.East), (end, endDirection)).getWeight.toInt)
    }
  }
}
