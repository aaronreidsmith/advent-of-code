package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.extensions.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}
import org.jgrapht.alg.shortestpath.AllDirectedPaths
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.io.Source
import scala.jdk.CollectionConverters.*

// Brute force, very slow, but works
@Slow(part1 = true, part2 = true)
object Day23 extends Solution {
  type I  = Grid[Char]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Grid[Char] = file.toGrid

  override def part1(input: Grid[Char]): Int = solution(
    input,
    grid => {
      val graph = new DefaultDirectedGraph[Point, DefaultEdge](classOf[DefaultEdge])
      grid.foreach { (point, char) =>
        char match {
          case '.' =>
            graph.addVertex(point)
            grid.getOrElse(point.up, '#') match {
              case '.' =>
                graph.addVertex(point.up)
                graph.addEdge(point, point.up)
                graph.addEdge(point.up, point)
              case '^' =>
                graph.addVertex(point.up)
                graph.addEdge(point, point.up)
              case _ => // do nothing
            }
            grid.getOrElse(point.right, '#') match {
              case '.' =>
                graph.addVertex(point.right)
                graph.addEdge(point, point.right)
                graph.addEdge(point.right, point)
              case '>' =>
                graph.addVertex(point.right)
                graph.addEdge(point, point.right)
              case _ => // do nothing
            }
            grid.getOrElse(point.down, '#') match {
              case '.' =>
                graph.addVertex(point.down)
                graph.addEdge(point, point.down)
                graph.addEdge(point.down, point)
              case 'v' =>
                graph.addVertex(point.down)
                graph.addEdge(point, point.down)
              case _ => // do nothing
            }
            grid.getOrElse(point.left, '#') match {
              case '.' =>
                graph.addVertex(point.left)
                graph.addEdge(point, point.left)
                graph.addEdge(point.left, point)
              case '<' =>
                graph.addVertex(point.left)
                graph.addEdge(point, point.left)
              case _ => // do nothing
            }
          case '^' =>
            graph.addVertex(point)
            graph.addVertex(point.up)
            graph.addEdge(point, point.up)
          case '>' =>
            graph.addVertex(point)
            graph.addVertex(point.right)
            graph.addEdge(point, point.right)
          case 'v' =>
            graph.addVertex(point)
            graph.addVertex(point.down)
            graph.addEdge(point, point.down)
          case '<' =>
            graph.addVertex(point)
            graph.addVertex(point.left)
            graph.addEdge(point, point.left)
          case _ => // do nothing
        }
      }
      val nonWalls = grid.collect { case (point, char) if char != '#' => point }
      val start    = nonWalls.min
      val end      = nonWalls.max

      (start, end, graph)
    }
  )

  override def part2(input: Grid[Char]): Int = solution(
    input,
    grid => {
      val graph = new DefaultDirectedGraph[Point, DefaultEdge](classOf[DefaultEdge])
      grid.foreach { (point, char) =>
        if (char != '#') {
          graph.addVertex(point)
          point.immediateNeighbors.foreach { neighbor =>
            if (grid.contains(neighbor) && grid(neighbor) != '#') {
              graph.addVertex(neighbor)
              graph.addEdge(point, neighbor)
              graph.addEdge(neighbor, point)
            }
          }
        }
      }
      val nonWalls = grid.collect { case (point, char) if char != '#' => point }
      val start    = nonWalls.min
      val end      = nonWalls.max

      (start, end, graph)
    }
  )

  private def solution(
      grid: Grid[Char],
      parse: Grid[Char] => (Point, Point, DefaultDirectedGraph[Point, DefaultEdge])
  ): Int = {
    val (start, end, graph) = parse(grid)
    val paths               = new AllDirectedPaths(graph)
    paths.getAllPaths(start, end, true, null).asScala.foldLeft(0)((acc, path) => acc.max(path.getLength))
  }
}
