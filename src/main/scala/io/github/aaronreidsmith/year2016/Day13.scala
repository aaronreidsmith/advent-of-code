package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Point, Solution, using}
import org.jgrapht.alg.shortestpath.{AllDirectedPaths, DijkstraShortestPath}
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source
import scala.jdk.CollectionConverters._

object Day13 extends Solution {
  type I  = DefaultUndirectedGraph[Point, DefaultEdge]
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2016, Day 13")
    val input = using("2016/day13.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def parseInput(file: Source): DefaultUndirectedGraph[Point, DefaultEdge] = {
    val input = file.mkString.toInt
    val graph = new DefaultUndirectedGraph[Point, DefaultEdge](classOf[DefaultEdge])

    // Add all our vertices
    for {
      x <- 0 to 50
      y <- 0 to 50
      num      = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + input
      oneCount = num.toBinaryString.count(_ == '1')
      if oneCount % 2 == 0
    } {
      graph.addVertex(Point(x, y))
    }

    // Add all our edges
    for {
      point    <- graph.vertexSet().asScala
      neighbor <- point.immediateNeighbors
      if graph.containsVertex(neighbor)
    } {
      graph.addEdge(point, neighbor)
    }

    graph
  }

  override protected[year2016] def part1(input: DefaultUndirectedGraph[Point, DefaultEdge]): Int = {
    DijkstraShortestPath.findPathBetween(input, Point(1, 1), Point(31, 39)).getLength
  }

  override protected[year2016] def part2(input: DefaultUndirectedGraph[Point, DefaultEdge]): Int = {
    val start = Point(1, 1)
    input.vertexSet().asScala.count { other =>
      Option(DijkstraShortestPath.findPathBetween(input, start, other)).fold(false)(_.getLength <= 50)
    }
  }
}
