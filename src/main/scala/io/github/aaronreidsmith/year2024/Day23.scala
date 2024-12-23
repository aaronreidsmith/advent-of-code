package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution
import org.jgrapht.alg.clique.BronKerboschCliqueFinder
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source
import scala.jdk.CollectionConverters.*

object Day23 extends Solution {
  type I  = DefaultUndirectedGraph[String, DefaultEdge]
  type O1 = Int
  type O2 = String

  override def parseInput(file: Source): DefaultUndirectedGraph[String, DefaultEdge] = {
    val graph = new DefaultUndirectedGraph[String, DefaultEdge](classOf[DefaultEdge])
    file.getLines().foreach { line =>
      val Array(left, right, _*) = line.split('-'): @unchecked
      graph.addVertex(left)
      graph.addVertex(right)
      graph.addEdge(left, right)
    }
    graph
  }

  override def part1(input: DefaultUndirectedGraph[String, DefaultEdge]): Int = {
    val cliqueFinder = BronKerboschCliqueFinder(input)
    cliqueFinder.asScala
      .foldLeft(Set.empty[Set[String]]) { (acc, jClique) =>
        val clique = jClique.asScala
        if (clique.size >= 3) {
          acc ++ clique.toSeq.combinations(3).collect {
            case triple if triple.exists(_.startsWith("t")) => triple.toSet
          }
        } else {
          acc
        }
      }
      .size
  }

  override def part2(input: DefaultUndirectedGraph[String, DefaultEdge]): String = {
    val cliqueFinder = BronKerboschCliqueFinder(input)
    cliqueFinder.asScala.maxBy(_.size()).asScala.toSeq.sorted.mkString(",")
  }
}
