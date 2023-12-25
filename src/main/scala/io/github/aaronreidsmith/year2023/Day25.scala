package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution
import org.jgrapht.alg.flow.GusfieldGomoryHuCutTree
import org.jgrapht.graph.{DefaultUndirectedWeightedGraph, DefaultWeightedEdge}

import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Random

object Day25 extends Solution {
  type I  = DefaultUndirectedWeightedGraph[String, DefaultWeightedEdge]
  type O1 = Int
  type O2 = Nothing

  override def parseInput(file: Source): DefaultUndirectedWeightedGraph[String, DefaultWeightedEdge] = {
    val graph = new DefaultUndirectedWeightedGraph[String, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    file.getLines().foreach { line =>
      val Array(from, toRaw, _*) = line.split(": "): @unchecked
      graph.addVertex(from)
      toRaw.split(' ').foreach { to =>
        graph.addVertex(to)
        graph.addEdge(from, to)
        graph.setEdgeWeight(from, to, 1.0)
      }
    }
    graph
  }

  override def part1(input: DefaultUndirectedWeightedGraph[String, DefaultWeightedEdge]): Int = {
    val keys       = input.vertexSet().asScala.toList
    val minimumCut = new GusfieldGomoryHuCutTree(input)

    var cutValue = 100d
    while (cutValue > 3) {
      val source = Random.shuffle(keys).head
      val sink   = Random.shuffle(keys).head

      if (source != sink) {
        cutValue = minimumCut.calculateMinCut(source, sink)
      }
    }
    minimumCut.getSourcePartition.size() * minimumCut.getSinkPartition.size()
  }
}
