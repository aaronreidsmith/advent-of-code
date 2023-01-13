package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Point, Solution}
import org.jgrapht.alg.shortestpath.BFSShortestPath
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source
import scala.jdk.CollectionConverters._

object Day13 extends Solution {
  type I  = DefaultUndirectedGraph[Point, DefaultEdge]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): DefaultUndirectedGraph[Point, DefaultEdge] = {
    val input = file.mkString.trim.toInt
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

  override def part1(input: DefaultUndirectedGraph[Point, DefaultEdge]): Int = {
    BFSShortestPath.findPathBetween(input, Point(1, 1), Point(31, 39)).getLength
  }

  override def part2(input: DefaultUndirectedGraph[Point, DefaultEdge]): Int = {
    val start = Point(1, 1)
    input.vertexSet().asScala.count { other =>
      Option(BFSShortestPath.findPathBetween(input, start, other)) match {
        case Some(path) => path.getLength <= 50
        case None       => false
      }
    }
  }
}
