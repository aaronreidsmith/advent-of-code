package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.implicits.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source

// It's ugly, but it works 🤷
object Day20 extends Solution {
  type I  = Grid[Tile]
  type O1 = Int
  type O2 = Int

  enum Tile {
    case Wall
    case Path
    case Portal(label: String, isInterior: Boolean = false)
  }

  override def parseInput(file: Source): Map[Point, Tile] = {
    val raw = file.toGrid.withDefaultValue('#')
    raw
      .map {
        case (point, char) =>
          char match {
            case ' ' | '#' => point -> Tile.Wall
            case '.'       =>
              // TODO: There has to be a better way...
              val charAbove  = raw(point.up)
              val twoAbove   = raw(point.up.up)
              val threeAbove = raw(point.up.up.up)
              val charRight  = raw(point.right)
              val twoRight   = raw(point.right.right)
              val threeRight = raw(point.right.right.right)
              val charBelow  = raw(point.down)
              val twoBelow   = raw(point.down.down)
              val threeBelow = raw(point.down.down.down)
              val charLeft   = raw(point.left)
              val twoLeft    = raw(point.left.left)
              val threeLeft  = raw(point.left.left.left)
              if (charAbove.isLetter && twoAbove.isLetter) {
                val isInterior = threeAbove == ' '
                point -> Tile.Portal(s"$twoAbove$charAbove", isInterior)
              } else if (charRight.isLetter && twoRight.isLetter) {
                val isInterior = threeRight == ' '
                point -> Tile.Portal(s"$charRight$twoRight", isInterior)
              } else if (charBelow.isLetter && twoBelow.isLetter) {
                val isInterior = threeBelow == ' '
                point -> Tile.Portal(s"$charBelow$twoBelow", isInterior)
              } else if (charLeft.isLetter && twoLeft.isLetter) {
                val isInterior = threeLeft == ' '
                point -> Tile.Portal(s"$twoLeft$charLeft", isInterior)
              } else {
                point -> Tile.Path
              }
            case _ => point -> Tile.Wall
          }
      }
      .withDefaultValue(Tile.Wall)
  }

  override def part1(grid: Grid[Tile]): Int = {
    val graph = new DefaultUndirectedGraph[Point, DefaultEdge](classOf[DefaultEdge])
    grid.foreach {
      case (_, Tile.Wall) => // Skip walls
      case (point, tile) =>
        val possibleNeighbors = point.immediateNeighbors.filter(other => grid(other) != Tile.Wall) ++ {
          tile match {
            case Tile.Portal(label, _) if !Seq("AA", "ZZ").contains(label) =>
              val otherPortal = grid.collectFirst {
                case (otherPoint, Tile.Portal(otherLabel, _)) if label == otherLabel && point != otherPoint =>
                  otherPoint
              }.get
              Seq(otherPortal)
            case _ => Seq.empty[Point]
          }
        }
        graph.addVertex(point)
        possibleNeighbors.foreach(graph.addVertex)
        possibleNeighbors.foreach(graph.addEdge(point, _))
    }
    val start = grid.collectFirst { case (point, Tile.Portal(label, _)) if label == "AA" => point }.get
    val end   = grid.collectFirst { case (point, Tile.Portal(label, _)) if label == "ZZ" => point }.get
    DijkstraShortestPath.findPathBetween(graph, start, end).getLength
  }

  override def part2(grid: Grid[Tile]): Int = {
    val graph = new DefaultUndirectedGraph[(Int, Int, Int), DefaultEdge](classOf[DefaultEdge])
    (0 until 30).foreach { level =>
      grid.foreach {
        case (_, Tile.Wall) => // Skip walls
        case (point, tile) =>
          val (x, y) = point.asPair
          val possibleNeighbors = point.immediateNeighbors.collect {
            case neighbor if grid(neighbor) != Tile.Wall => (neighbor.x, neighbor.y, level)
          } ++ {
            tile match {
              case Tile.Portal(label, isInterior) if !Seq("AA", "ZZ").contains(label) =>
                val (x, y, otherLevel) = grid.collectFirst {
                  case (otherPoint, Tile.Portal(otherLabel, _)) if label == otherLabel && point != otherPoint =>
                    (otherPoint.x, otherPoint.y, if (isInterior) level + 1 else level - 1)
                }.get
                Seq((x, y, otherLevel)).filter(_._3 >= 0)
              case _ => Seq.empty[(Int, Int, Int)]
            }
          }
          graph.addVertex((x, y, level))
          possibleNeighbors.foreach { (nX, nY, nLevel) =>
            graph.addVertex((nX, nY, nLevel))
            graph.addEdge((x, y, level), (nX, nY, nLevel))
          }
      }
    }
    val (startX, startY) =
      grid.collectFirst { case (point, Tile.Portal(label, _)) if label == "AA" => point }.get.asPair
    val (endX, endY) = grid.collectFirst { case (point, Tile.Portal(label, _)) if label == "ZZ" => point }.get.asPair
    DijkstraShortestPath.findPathBetween(graph, (startX, startY, 0), (endX, endY, 0)).getLength
  }
}
