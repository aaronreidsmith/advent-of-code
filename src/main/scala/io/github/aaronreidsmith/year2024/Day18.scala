package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.{Point, Solution}
import org.jgrapht.alg.shortestpath.BFSShortestPath
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source

@Slow(part2 = true)
object Day18 extends Solution {
  type I  = List[Point]
  type O1 = Int
  type O2 = String

  private val target = {
    val max = if (isTest) 6 else 70
    Point(max, max)
  }
  private val firstBytes = if (isTest) 12 else 1024

  override def parseInput(file: Source): List[Point] = {
    file.getLines().toList.map { str =>
      val Array(x, y, _*) = str.split(','): @unchecked
      Point(x.toInt, y.toInt)
    }
  }

  override def part1(input: List[Point]): Int = {
    val graph = buildGraph(input.take(firstBytes).toSet)
    BFSShortestPath.findPathBetween(graph, Point.zero, target).getLength
  }

  override def part2(input: List[Point]): String = {
    // Work backwards to find first unblocked instead of working forwards to find first blocked
    val lastUnblocked = (input.length until firstBytes by -1).find { i =>
      val blocked = input.take(i).toSet
      val graph   = buildGraph(blocked)
      Option(BFSShortestPath.findPathBetween(graph, Point.zero, target)).isDefined
    }.get
    val Point(x, y) = input(lastUnblocked)
    s"$x,$y"
  }

  private def buildGraph(blocked: Set[Point]): DefaultUndirectedGraph[Point, DefaultEdge] = {
    val graph = DefaultUndirectedGraph[Point, DefaultEdge](classOf[DefaultEdge])

    for {
      x <- 0 to target.x
      y <- 0 to target.y
      point = Point(x, y)
      if !blocked.contains(point)
    } {
      graph.addVertex(point)
      point.immediateNeighbors.filterNot(blocked.contains).foreach { neighbor =>
        graph.addVertex(neighbor)
        graph.addEdge(point, neighbor)
        graph.addEdge(neighbor, point)
      }
    }

    graph
  }
}
