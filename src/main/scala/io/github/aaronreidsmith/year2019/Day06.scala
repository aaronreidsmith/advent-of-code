package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source
import scala.jdk.CollectionConverters._

object Day06 extends Solution {
  type I  = DefaultUndirectedGraph[String, DefaultEdge]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): DefaultUndirectedGraph[String, DefaultEdge] = {
    val graph = new DefaultUndirectedGraph[String, DefaultEdge](classOf[DefaultEdge])
    file.getLines().foreach { line =>
      val Array(inner, outer, _*) = line.split(')')
      graph.addVertex(inner)
      graph.addVertex(outer)
      graph.addEdge(outer, inner)
    }
    graph
  }

  override def part1(orbits: DefaultUndirectedGraph[String, DefaultEdge]): Int = {
    orbits.vertexSet().asScala.foldLeft(0) { (acc, vertex) =>
      acc + DijkstraShortestPath.findPathBetween(orbits, vertex, "COM").getLength
    }
  }

  override def part2(orbits: DefaultUndirectedGraph[String, DefaultEdge]): Int = {
    DijkstraShortestPath.findPathBetween(orbits, "YOU", "SAN").getLength - 2
  }
}
