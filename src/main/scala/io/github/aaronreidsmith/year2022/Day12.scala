package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Grid, Point, using}
import io.github.aaronreidsmith.implicits.SourceOps
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source
import scala.jdk.CollectionConverters._

object Day12 {
  private implicit class CharOps(char: Char) {
    def elevation: Char = char match {
      case 'S'   => 'a'
      case 'E'   => 'z'
      case other => other
    }
  }

  def main(args: Array[String]): Unit = {
    val input = using("2022/day12.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): DefaultDirectedGraph[(Point, Char), DefaultEdge] = {
    val graph = new DefaultDirectedGraph[(Point, Char), DefaultEdge](classOf[DefaultEdge])
    val grid  = file.toGrid
    grid.foreach {
      case (point, char) =>
        // Always add this vertex
        graph.addVertex((point, char))

        // Only add neighbor vertices/edges if we can reach them
        for {
          neighbor     <- point.immediateNeighbors
          neighborChar <- grid.get(neighbor)
          if neighborChar.elevation <= char.elevation + 1
        } {
          graph.addVertex((neighbor, neighborChar))
          graph.addEdge((point, char), (neighbor, neighborChar))
        }
    }

    graph
  }

  protected[year2022] def part1(input: DefaultDirectedGraph[(Point, Char), DefaultEdge]): Int = {
    val vertices = input.vertexSet().asScala
    val start    = vertices.find(_._2 == 'S').get
    val end      = vertices.find(_._2 == 'E').get
    DijkstraShortestPath.findPathBetween(input, start, end).getLength
  }

  protected[year2022] def part2(input: DefaultDirectedGraph[(Point, Char), DefaultEdge]): Int = {
    val vertices = input.vertexSet().asScala
    val end      = vertices.find(_._2 == 'E').get
    vertices.foldLeft(Int.MaxValue) {
      case (currentMin, candidate) if candidate._2.elevation == 'a' =>
        Option(DijkstraShortestPath.findPathBetween(input, candidate, end)) match {
          case Some(path) => currentMin.min(path.getLength)
          case None       => currentMin
        }
      case (currentMin, _) => currentMin
    }
  }
}
