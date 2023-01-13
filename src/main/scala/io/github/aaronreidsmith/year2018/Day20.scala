package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.{Direction, Point, Solution}
import org.jgrapht.alg.shortestpath.BFSShortestPath
import org.jgrapht.graph.{DefaultEdge, SimpleDirectedGraph}

import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters._

// Adapted from https://www.reddit.com/r/adventofcode/comments/a7uk3f/2018_day_20_solutions/ec5y3lm/
@Slow(parsing = true)
object Day20 extends Solution {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[Int] = {
    val paths    = file.mkString.tail.init
    val graph    = new SimpleDirectedGraph[Point, DefaultEdge](classOf[DefaultEdge])
    val start    = Point.zero
    val position = mutable.Set(start)
    val stack    = mutable.Stack.empty[(Set[Point], Set[Point])]
    val starts   = mutable.Set(start)
    val ends     = mutable.Set.empty[Point]
    paths.foreach {
      case '|' =>
        ends.addAll(position)
        position.clear()
        position.addAll(starts)
      case char if "NESW".contains(char) =>
        val newPos = position.map { source =>
          val dest = source.move(Direction.fromChar(char))
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
      BFSShortestPath.findPathBetween(graph, start, vertex).getLength :: acc
    }
  }

  override def part1(lengths: List[Int]): Int = lengths.max
  override def part2(lengths: List[Int]): Int = lengths.count(_ >= 1000)
}
