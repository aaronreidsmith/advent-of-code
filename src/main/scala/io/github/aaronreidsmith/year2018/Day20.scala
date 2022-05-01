package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.using
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters._

// Adapted from https://www.reddit.com/r/adventofcode/comments/a7uk3f/2018_day_20_solutions/ec5y3lm/
object Day20 {
  private type Node = (Int, Int)

  def main(args: Array[String]): Unit = {
    val pathLengths = using("2018/day20.txt")(parseInput)
    println(s"Part 1: ${part1(pathLengths)}")
    println(s"Part 2: ${part2(pathLengths)}")
  }

  private[year2018] def parseInput(file: Source): List[Int] = {
    val paths    = file.mkString.tail.init
    val graph    = new DefaultDirectedGraph[Node, DefaultEdge](classOf[DefaultEdge])
    val start    = (0, 0)
    val position = mutable.Set(start)
    val stack    = mutable.Stack.empty[(Set[Node], Set[Node])]
    val starts   = mutable.Set(start)
    val ends     = mutable.Set.empty[Node]
    paths.foreach {
      case '|' =>
        ends.addAll(position)
        position.clear()
        position.addAll(starts)
      case char if "NESW".contains(char) =>
        val (dx, dy) = char match {
          case 'N' => (-1, 0)
          case 'E' => (0, 1)
          case 'S' => (1, 0)
          case 'W' => (0, -1)
          case _   => throw new IllegalArgumentException
        }
        val newPos = position.map {
          case (x, y) =>
            val source = (x, y)
            val dest   = (x + dx, y + dy)
            graph.addVertex(source)
            graph.addVertex(dest)
            graph.addEdge(source, dest)
            dest
        }
        position.clear()
        position.addAll(newPos)
      case '(' =>
        stack.push((starts.toSet, ends.toSet))
        starts.clear()
        starts.addAll(position)
        ends.clear()
      case ')' =>
        position.addAll(ends)
        val popped = stack.pop()
        starts.clear()
        starts.addAll(popped._1)
        ends.clear()
        ends.addAll(popped._2)
      case _ => throw new IllegalArgumentException
    }
    graph.vertexSet().asScala.foldLeft(List.empty[Int]) { (acc, vertex) =>
      DijkstraShortestPath.findPathBetween(graph, start, vertex).getLength :: acc
    }
  }

  private[year2018] def part1(lengths: List[Int]): Int = lengths.max
  private[year2018] def part2(lengths: List[Int]): Int = lengths.count(_ >= 1000)
}
