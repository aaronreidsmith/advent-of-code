package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{Direction, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day12 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  case class Ship(position: Point, heading: Direction, waypoint: Point) {
    // Part 1 methods
    def turn(instruction: String): Ship = {
      val direction = instruction.head
      val degrees   = instruction.tail.toInt
      turn(direction, degrees)
    }

    private def turn(direction: Char, degrees: Int): Ship = {
      val newHeading = (direction, degrees) match {
        case ('L', 90) | ('R', 270) => heading.left
        case ('L', 270) | ('R', 90) => heading.right
        case (_, 180)               => heading.opposite
        case _                      => throw new IllegalArgumentException
      }
      this.copy(heading = newHeading)
    }

    def move(instruction: String): Ship = {
      val direction = Direction.fromChar(instruction.head)
      val steps     = instruction.tail.toInt
      this.copy(position = position.move(direction, steps))
    }

    def moveForward(steps: Int): Ship = this.copy(position = position.move(heading, steps))

    // Part 2 methods
    def rotateWaypoint(instruction: String): Ship = {
      val direction = instruction.head
      val degrees   = instruction.tail.toInt
      rotateWaypoint(direction, degrees)
    }

    @tailrec
    final def rotateWaypoint(direction: Char, degrees: Int): Ship = {
      val Point(x, y) = waypoint
      if (degrees == 0) {
        this
      } else {
        val newDegrees = degrees - 90
        direction match {
          case 'L' => this.copy(waypoint = Point(-y, x)).rotateWaypoint(direction, newDegrees)
          case 'R' => this.copy(waypoint = Point(y, -x)).rotateWaypoint(direction, newDegrees)
          case _   => throw new IllegalArgumentException
        }
      }
    }

    def moveWaypoint(instruction: String): Ship = {
      val direction = Direction.fromChar(instruction.head)
      val steps     = instruction.tail.toInt
      this.copy(waypoint = waypoint.move(direction, steps))
    }

    def moveAroundWaypoint(numberOfMoves: Int): Ship = {
      val Point(x, y) = position
      val Point(i, j) = waypoint

      val newX = x + (numberOfMoves * i)
      val newY = y + (numberOfMoves * j)
      this.copy(position = Point(newX, newY))
    }
  }

  object Ship {
    def initial: Ship = Ship(Point.zero, Direction.East, Point(-1, 10))
  }

  override def parseInput(file: Source): List[String] = file.getLines().toList
  override def part1(input: List[String]): Int        = solution(input, part2 = false)
  override def part2(input: List[String]): Int        = solution(input, part2 = true)

  private def solution(input: List[String], part2: Boolean): Int = {
    @tailrec
    def helper(directions: List[String], ship: Ship = Ship.initial): Int = directions match {
      case Nil => ship.position.manhattanDistance(Point.zero)
      case instruction :: tail =>
        val updatedShip = instruction.head match {
          case 'N' | 'E' | 'S' | 'W' => if (part2) ship.moveWaypoint(instruction) else ship.move(instruction)
          case 'L' | 'R'             => if (part2) ship.rotateWaypoint(instruction) else ship.turn(instruction)
          case 'F' =>
            val steps = instruction.tail.toInt
            if (part2) ship.moveAroundWaypoint(steps) else ship.moveForward(steps)
          case _ => throw new IllegalArgumentException
        }
        helper(tail, updatedShip)
    }

    helper(input)
  }
}
