package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.using
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source
import scala.jdk.CollectionConverters._

object Day06 {
  def main(args: Array[String]): Unit = {
    val orbits = using("2019/day06.txt")(parseInput)
    println(s"Part 1: ${part1(orbits)}")
    println(s"Part 2: ${part2(orbits)}")
  }

  private[year2019] def parseInput(file: Source): DefaultUndirectedGraph[String, DefaultEdge] = {
    val graph = new DefaultUndirectedGraph[String, DefaultEdge](classOf[DefaultEdge])
    file.getLines().foreach { line =>
      val Array(inner, outer, _*) = line.split(')')
      graph.addVertex(inner)
      graph.addVertex(outer)
      graph.addEdge(outer, inner)
    }
    graph
  }

  private[year2019] def part1(orbits: DefaultUndirectedGraph[String, DefaultEdge]): Int = {
    orbits.vertexSet().asScala.foldLeft(0) { (acc, vertex) =>
      acc + DijkstraShortestPath.findPathBetween(orbits, vertex, "COM").getLength
    }
  }

  private[year2019] def part2(orbits: DefaultUndirectedGraph[String, DefaultEdge]): Int = {
    DijkstraShortestPath.findPathBetween(orbits, "YOU", "SAN").getLength - 2
  }
}
