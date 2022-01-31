package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils
import io.github.aaronreidsmith.year2019.intcode.{Instructions, IntCode}
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultEdge, SimpleGraph}

import scala.util.Random

object Day15 extends IntCodeUtils {
  private val NORTH      = 1L
  private val SOUTH      = 2L
  private val WEST       = 3L
  private val EAST       = 4L
  private val DIRECTIONS = Seq(NORTH, SOUTH, WEST, EAST)

  private val WALL   = 0L
  private val SPACE  = 1L
  private val TARGET = 2L

  private val graph = new SimpleGraph[(Int, Int), DefaultEdge](classOf[DefaultEdge])

  private var farthestDistance          = 0
  private var graphBuilt                = false
  private var intCode: IntCode          = _
  private var target: (Int, Int)        = _
  private var farthestPoint: (Int, Int) = _

  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day15.txt")
    println(s"Part 1: ${part1(instructions)}")
    println(s"Part 2: ${part2(instructions)}")
  }

  private[year2019] def part1(instructions: Instructions): Int = {
    if (!graphBuilt) {
      buildGraph(instructions)
    }
    DijkstraShortestPath.findPathBetween(graph, (0, 0), target).getLength
  }

  private[year2019] def part2(instructions: Instructions): Int = {
    if (!graphBuilt) {
      buildGraph(instructions)
    }
    DijkstraShortestPath.findPathBetween(graph, (0, 0), farthestPoint).getLength
  }

  private def buildGraph(instructions: Instructions): Unit = {
    intCode = new IntCode(instructions, suspendOnOutput = true)
    var position        = (0, 0)
    var currentResponse = 0L
    while (currentResponse != TARGET) {
      val choice = Random.shuffle(DIRECTIONS).head
      currentResponse = intCode.run(Seq(choice)).getOutput.last
      currentResponse match {
        case WALL => // Do nothing
        case SPACE | TARGET =>
          val (row, col) = position
          val nextPosition = choice match {
            case NORTH => (row - 1, col)
            case SOUTH => (row + 1, col)
            case WEST  => (row, col - 1)
            case EAST  => (row, col + 1)
            case _     => throw new IllegalArgumentException
          }
          graph.addVertex(position)
          graph.addVertex(nextPosition)
          graph.addEdge(position, nextPosition)
          graph.addEdge(nextPosition, position)

          val (nextRow, nextCol) = nextPosition
          val distance           = nextRow.abs + nextCol.abs
          if (distance > farthestDistance) {
            farthestPoint = nextPosition
            farthestDistance = distance
          }

          position = nextPosition
        case _ => throw new IllegalArgumentException
      }
    }
    target = position
    graphBuilt = true
  }
}
