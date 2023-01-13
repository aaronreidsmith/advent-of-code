package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution
import org.jgrapht.alg.shortestpath.AllDirectedPaths
import org.jgrapht.graph.{DefaultWeightedEdge, DefaultDirectedWeightedGraph}

import scala.io.Source
import scala.jdk.CollectionConverters._

object Day09 extends Solution {
  type I  = DefaultDirectedWeightedGraph[String, DefaultWeightedEdge]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): DefaultDirectedWeightedGraph[String, DefaultWeightedEdge] = {
    val entry = "^(.*) to (.*) = (\\d+)$".r
    val graph = new DefaultDirectedWeightedGraph[String, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    file.getLines().foreach {
      case entry(start, end, weight) =>
        // Make sure we have both vertices
        graph.addVertex(start)
        graph.addVertex(end)

        // x to y
        graph.addEdge(start, end)
        graph.setEdgeWeight(start, end, weight.toDouble)

        // y to x
        graph.addEdge(end, start)
        graph.setEdgeWeight(end, start, weight.toDouble)
      case _ => // Do nothing
    }
    graph
  }

  // format: off
  override def part1(graph: DefaultDirectedWeightedGraph[String, DefaultWeightedEdge]): Int = allPathWeights(graph).min.toInt
  override def part2(graph: DefaultDirectedWeightedGraph[String, DefaultWeightedEdge]): Int = allPathWeights(graph).max.toInt
  // format: on

  private def allPathWeights(graph: DefaultDirectedWeightedGraph[String, DefaultWeightedEdge]): Vector[Double] = {
    val nodes            = graph.vertexSet().asScala
    val targetPathLength = nodes.size - 1 // edges = nodes - 1
    val allDirectedPaths = new AllDirectedPaths(graph)
    nodes.foldLeft(Vector.empty[Double]) { (allPaths, node) =>
      val otherNodes = nodes.filterNot(_ == node)
      val paths = otherNodes.foldLeft(Vector.empty[Double]) { (pathsFromNode, other) =>
        val newPaths = allDirectedPaths.getAllPaths(node, other, true, targetPathLength).asScala.collect {
          case path if path.getLength == targetPathLength => path.getWeight
        }
        pathsFromNode ++ newPaths
      }
      allPaths ++ paths
    }
  }
}
