package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

object Day12 {
  private sealed trait TurnDirection
  private case object Left  extends TurnDirection
  private case object Right extends TurnDirection

  private sealed trait Heading {
    def turn(direction: TurnDirection): Heading = direction match {
      case Left  => left
      case Right => right
    }
    val left: Heading
    val right: Heading
  }
  private case object North extends Heading {
    val left: Heading  = West
    val right: Heading = East
  }
  private case object East extends Heading {
    val left: Heading  = North
    val right: Heading = South
  }
  private case object South extends Heading {
    val left: Heading  = East
    val right: Heading = West
  }
  private case object West extends Heading {
    val left: Heading  = South
    val right: Heading = North
  }

  private case class Ship(position: (Int, Int) = (0, 0), heading: Heading = East, waypoint: (Int, Int) = (-1, 10)) {
    private val (x, y) = position
    private val (i, j) = waypoint

    // Part 1 methods
    def turn(turnDirection: TurnDirection, degrees: Int): Ship = {
      val newHeading = degrees match {
        case 90  => heading.turn(turnDirection)
        case 180 => heading.turn(turnDirection).turn(turnDirection)
        case 270 => heading.turn(turnDirection).turn(turnDirection).turn(turnDirection)
        case _   => throw new IllegalArgumentException
      }
      this.copy(heading = newHeading)
    }
    def move(value: Int): Ship = move(heading, value)
    def move(heading: Heading, value: Int): Ship = heading match {
      case North => this.copy(position = (x - value, y))
      case East  => this.copy(position = (x, y + value))
      case South => this.copy(position = (x + value, y))
      case West  => this.copy(position = (x, y - value))
    }

    // Part 2 methods
    def rotateWaypoint(direction: TurnDirection, degrees: Int): Ship = if (degrees == 0) {
      this
    } else {
      val newDegrees = degrees - 90
      direction match {
        case Left  => this.copy(waypoint = (-j, i)).rotateWaypoint(direction, newDegrees)
        case Right => this.copy(waypoint = (j, -i)).rotateWaypoint(direction, newDegrees)
      }
    }
    def moveWaypoint(heading: Heading, value: Int): Ship = heading match {
      case North => this.copy(waypoint = (i - value, j))
      case East  => this.copy(waypoint = (i, j + value))
      case South => this.copy(waypoint = (i + value, j))
      case West  => this.copy(waypoint = (i, j - value))
    }
    def moveAroundWaypoint(numberOfMoves: Int): Ship = {
      val newX = x + (numberOfMoves * i)
      val newY = y + (numberOfMoves * j)
      this.copy(position = (newX, newY))
    }

    // Common methods
    def distanceFromOrigin: Int = x.abs + y.abs
  }

  private val north = "^N(\\d+)$".r
  private val east  = "^E(\\d+)$".r
  private val south = "^S(\\d+)$".r
  private val west  = "^W(\\d+)$".r

  private val left    = "^L(\\d+)$".r
  private val right   = "^R(\\d+)$".r
  private val forward = "^F(\\d+)$".r

  def main(args: Array[String]): Unit = {
    val input = using("2020/day12.txt")(_.getLines().toList)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  @tailrec
  private def part1(directions: List[String], ship: Ship = Ship()): Int = directions match {
    case Nil => ship.distanceFromOrigin
    case head :: tail =>
      val updatedShip = head match {
        case north(value)   => ship.move(North, value.toInt)
        case east(value)    => ship.move(East, value.toInt)
        case south(value)   => ship.move(South, value.toInt)
        case west(value)    => ship.move(West, value.toInt)
        case left(value)    => ship.turn(Left, value.toInt)
        case right(value)   => ship.turn(Right, value.toInt)
        case forward(value) => ship.move(value.toInt)
      }
      part1(tail, updatedShip)
  }

  @tailrec
  private def part2(directions: List[String], ship: Ship = Ship()): Int = directions match {
    case Nil => ship.distanceFromOrigin
    case head :: tail =>
      val updatedShip = head match {
        case north(value)   => ship.moveWaypoint(North, value.toInt)
        case east(value)    => ship.moveWaypoint(East, value.toInt)
        case south(value)   => ship.moveWaypoint(South, value.toInt)
        case west(value)    => ship.moveWaypoint(West, value.toInt)
        case left(value)    => ship.rotateWaypoint(Left, value.toInt)
        case right(value)   => ship.rotateWaypoint(Right, value.toInt)
        case forward(value) => ship.moveAroundWaypoint(value.toInt)
      }
      part2(tail, updatedShip)
  }
}
