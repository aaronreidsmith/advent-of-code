package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{Point, Solution}
import io.github.aaronreidsmith.implicits.toGrid
import org.jgrapht.alg.shortestpath.{BFSShortestPath, DijkstraShortestPath}
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.io.Source
import scala.jdk.CollectionConverters.*

object Day12 extends Solution {
  type I  = DefaultDirectedGraph[(Point, Char), DefaultEdge]
  type O1 = Int
  type O2 = Int

  extension (char: Char) {
    def elevation: Char = char match {
      case 'S'   => 'a'
      case 'E'   => 'z'
      case other => other
    }
  }

  override def parseInput(file: Source): DefaultDirectedGraph[(Point, Char), DefaultEdge] = {
    val graph = new DefaultDirectedGraph[(Point, Char), DefaultEdge](classOf[DefaultEdge])
    val grid  = file.toGrid
    grid.foreach { (point, char) =>
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

  override def part1(input: DefaultDirectedGraph[(Point, Char), DefaultEdge]): Int = {
    val vertices = input.vertexSet().asScala
    val start    = vertices.find(_._2 == 'S').get
    val end      = vertices.find(_._2 == 'E').get
    DijkstraShortestPath.findPathBetween(input, start, end).getLength
  }

  override def part2(input: DefaultDirectedGraph[(Point, Char), DefaultEdge]): Int = {
    val vertices = input.vertexSet().asScala
    val end      = vertices.find(_._2 == 'E').get
    vertices.foldLeft(Int.MaxValue) {
      case (currentMin, candidate) if candidate._2.elevation == 'a' =>
        Option(BFSShortestPath.findPathBetween(input, candidate, end)) match {
          case Some(path) => currentMin.min(path.getLength)
          case None       => currentMin
        }
      case (currentMin, _) => currentMin
    }
  }
}
