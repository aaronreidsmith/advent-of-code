package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.implicits.SourceOps
import io.github.aaronreidsmith.{Point, Solution}
import org.jgrapht.alg.shortestpath.BFSShortestPath
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source
import scala.jdk.CollectionConverters._

// The majority of this is adapted from https://www.reddit.com/r/adventofcode/comments/5k1he1/comment/dbknd6b
object Day24 extends Solution(2016, 24) {
  type I  = DefaultUndirectedGraph[(Point, Char), DefaultEdge]
  type O1 = Int
  type O2 = Int

  override protected[year2016] def parseInput(file: Source): DefaultUndirectedGraph[(Point, Char), DefaultEdge] = {
    val grid  = file.toGrid
    val graph = new DefaultUndirectedGraph[(Point, Char), DefaultEdge](classOf[DefaultEdge])
    grid.foreach {
      case (point, char) if char != '#' =>
        graph.addVertex((point, char))
        point.immediateNeighbors.foreach { neighbor =>
          val neighborValue = grid(neighbor)
          if (neighborValue != '#') {
            graph.addVertex((neighbor, neighborValue))
            graph.addEdge((point, char), (neighbor, neighborValue))
          }
        }
      case _ => // do nothing
    }
    graph
  }

  override protected[year2016] def part1(input: DefaultUndirectedGraph[(Point, Char), DefaultEdge]): Int = {
    solution(input)._1
  }
  override protected[year2016] def part2(input: DefaultUndirectedGraph[(Point, Char), DefaultEdge]): Int = {
    solution(input)._2
  }

  // Both solutions require the same traversal, so might as well only do it once
  private var solved = false
  private var answer = (0, 0)
  private def solution(graph: DefaultUndirectedGraph[(Point, Char), DefaultEdge]): (Int, Int) = {
    if (!solved) {
      val vertices          = graph.vertexSet().asScala
      val start             = vertices.find { case (_, char) => char == '0' }.get
      val targets           = vertices.filter { case (_, char) => char != '.' && char != '0' }.toVector.sortBy(_._2)
      val distancesFromZero = targets.map(target => BFSShortestPath.findPathBetween(graph, start, target).getLength)
      val segments          = distancesFromZero.size
      val allDistances = {
        for {
          i <- 0 until segments
          j <- i + 1 until segments
          distance = BFSShortestPath.findPathBetween(graph, targets(i), targets(j)).getLength
        } yield Seq((i, j) -> distance, (j, i) -> distance)
      }.flatten.toMap

      answer = (0 until segments).permutations.foldLeft((Int.MaxValue, Int.MaxValue)) {
        case ((part1Acc, part2Acc), path) =>
          val distance = path.sliding(2).foldLeft(distancesFromZero(path.head)) {
            case (acc, Seq(a, b)) => acc + allDistances((a, b))
            case (acc, _)         => acc
          }
          (part1Acc.min(distance), part2Acc.min(distance + distancesFromZero(path.last)))
        case (acc, _) => acc
      }
      solved = true
    }

    answer
  }
}
