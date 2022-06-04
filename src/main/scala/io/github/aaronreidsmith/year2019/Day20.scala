package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{Point, using}
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultEdge, SimpleGraph}

import scala.io.Source

// It's ugly, but it works 🤷
object Day20 {
  private[year2019] sealed trait Tile
  private[year2019] case object Wall                                              extends Tile
  private[year2019] case object Path                                              extends Tile
  private[year2019] case class Portal(label: String, isInterior: Boolean = false) extends Tile

  def main(args: Array[String]): Unit = {
    val grid = using("2019/day20.txt")(parseInput)
    println(s"Part 1: ${part1(grid)}")
    println(s"Part 2: ${part2(grid)}")
  }

  private[year2019] def parseInput(file: Source): Map[Point, Tile] = {
    val raw = {
      for {
        (line, row) <- file.getLines().zipWithIndex
        (char, col) <- line.zipWithIndex
      } yield {
        (row, col) -> char
      }
    }.toMap.withDefaultValue('#')
    raw
      .map {
        case ((row, col), char) =>
          char match {
            case ' ' | '#' => Point(row, col) -> Wall
            case '.'       =>
              // TODO: There has to be a better way...
              val charAbove  = raw((row - 1, col))
              val twoAbove   = raw((row - 2, col))
              val threeAbove = raw((row - 3, col))
              val charRight  = raw((row, col + 1))
              val twoRight   = raw((row, col + 2))
              val threeRight = raw((row, col + 3))
              val charBelow  = raw((row + 1, col))
              val twoBelow   = raw((row + 2, col))
              val threeBelow = raw((row + 3, col))
              val charLeft   = raw((row, col - 1))
              val twoLeft    = raw((row, col - 2))
              val threeLeft  = raw((row, col - 3))
              if (charAbove.isLetter && twoAbove.isLetter) {
                val isInterior = threeAbove == ' '
                Point(row, col) -> Portal(s"$twoAbove$charAbove", isInterior)
              } else if (charRight.isLetter && twoRight.isLetter) {
                val isInterior = threeRight == ' '
                Point(row, col) -> Portal(s"$charRight$twoRight", isInterior)
              } else if (charBelow.isLetter && twoBelow.isLetter) {
                val isInterior = threeBelow == ' '
                Point(row, col) -> Portal(s"$charBelow$twoBelow", isInterior)
              } else if (charLeft.isLetter && twoLeft.isLetter) {
                val isInterior = threeLeft == ' '
                Point(row, col) -> Portal(s"$twoLeft$charLeft", isInterior)
              } else {
                Point(row, col) -> Path
              }
            case _ => Point(row, col) -> Wall
          }
      }
      .withDefaultValue(Wall)
  }

  private[year2019] def part1(grid: Map[Point, Tile]): Int = {
    val graph = new SimpleGraph[Point, DefaultEdge](classOf[DefaultEdge])
    grid.foreach {
      case (_, Wall) => // Skip walls
      case (point, tile) =>
        val possibleNeighbors = point.immediateNeighbors.filter(other => grid(other) != Wall) ++ {
          tile match {
            case Portal(label, _) if !Seq("AA", "ZZ").contains(label) =>
              val otherPortal = grid.collectFirst {
                case (otherPoint, Portal(otherLabel, _)) if label == otherLabel && point != otherPoint => otherPoint
              }.get
              Seq(otherPortal)
            case _ => Seq.empty[Point]
          }
        }
        graph.addVertex(point)
        possibleNeighbors.foreach(graph.addVertex)
        possibleNeighbors.foreach(graph.addEdge(point, _))
      case _ => throw new IllegalArgumentException
    }
    val start = grid.collectFirst { case (point, Portal(label, _)) if label == "AA" => point }.get
    val end   = grid.collectFirst { case (point, Portal(label, _)) if label == "ZZ" => point }.get
    DijkstraShortestPath.findPathBetween(graph, start, end).getLength
  }

  private[year2019] def part2(grid: Map[Point, Tile]): Int = {
    val graph = new SimpleGraph[(Int, Int, Int), DefaultEdge](classOf[DefaultEdge])
    (0 until 30).foreach { level =>
      grid.foreach {
        case (_, Wall) => // Skip walls
        case (point, tile) =>
          val (x, y) = point.unzip
          val possibleNeighbors = point.immediateNeighbors.collect {
            case neighbor if grid(neighbor) != Wall => (neighbor.x, neighbor.y, level)
          } ++ {
            tile match {
              case Portal(label, isInterior) if !Seq("AA", "ZZ").contains(label) =>
                val (x, y, otherLevel) = grid.collectFirst {
                  case (otherPoint, Portal(otherLabel, _)) if label == otherLabel && point != otherPoint =>
                    (otherPoint.x, otherPoint.y, if (isInterior) level + 1 else level - 1)
                }.get
                Seq((x, y, otherLevel)).filter(_._3 >= 0)
              case _ => Seq.empty[(Int, Int, Int)]
            }
          }
          graph.addVertex((x, y, level))
          possibleNeighbors.foreach {
            case (nX, nY, nLevel) =>
              graph.addVertex((nX, nY, nLevel))
              graph.addEdge((x, y, level), (nX, nY, nLevel))
          }
        case _ => throw new IllegalArgumentException
      }
    }
    val (startX, startY) = grid.collectFirst { case (point, Portal(label, _)) if label == "AA" => point }.get.unzip
    val (endX, endY)     = grid.collectFirst { case (point, Portal(label, _)) if label == "ZZ" => point }.get.unzip
    DijkstraShortestPath.findPathBetween(graph, (startX, startY, 0), (endX, endY, 0)).getLength
  }
}
