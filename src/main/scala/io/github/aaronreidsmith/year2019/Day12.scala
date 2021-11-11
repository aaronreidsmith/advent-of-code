package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.util.FileUtils

import scala.annotation.tailrec
import scala.io.Source

object Day12 extends FileUtils {
  private case class Point(x: Int, y: Int, z: Int) {
    def energy: Int = math.abs(x) + math.abs(y) + math.abs(z)
  }
  private case class Moon(position: Point, velocity: Point = Point(0, 0, 0)) {
    def potentialEnergy: Int = position.energy
    def kineticEnergy: Int   = velocity.energy
    def totalEnergy: Int     = potentialEnergy * kineticEnergy
  }

  private val moonEntry = "^<x=\\s*(-?\\d+), y=\\s*(-?\\d+), z=\\s*(-?\\d+)>$".r("x", "y", "z")

  def main(args: Array[String]): Unit = {
    val moons = using(Source.fromResource("2019/day12.txt")) { file =>
      file.getLines().foldLeft(List.empty[Moon]) { (acc, line) =>
        line match {
          case moonEntry(x, y, z) => acc :+ Moon(Point(x.toInt, y.toInt, z.toInt))
          case other              => throw new IllegalArgumentException(s"'$other' is not a valid entry'")
        }
      }
    }
    println(s"Part 1: ${part1(moons)}")
  }

  @tailrec
  private def part1(moons: List[Moon], iteration: Int = 0): Int = if (iteration >= 1000) {
    moons.foldLeft(0)(_ + _.totalEnergy)
  } else {
    part1(nextState(moons), iteration + 1)
  }

  private def nextState(moons: List[Moon]): List[Moon] = moons.map { moon =>
    val otherMoons = moons.filterNot(_ == moon)
    val withGravityApplied = otherMoons.foldLeft(moon) { (acc, otherMoon) =>
      val xChange = moon.position.x.compare(otherMoon.position.x)
      val yChange = moon.position.y.compare(otherMoon.position.y)
      val zChange = moon.position.z.compare(otherMoon.position.z)
      acc.copy(velocity = Point(acc.velocity.x - xChange, acc.velocity.y - yChange, acc.velocity.z - zChange))
    }
    withGravityApplied.copy(position =
      Point(
        withGravityApplied.position.x + withGravityApplied.velocity.x,
        withGravityApplied.position.y + withGravityApplied.velocity.y,
        withGravityApplied.position.z + withGravityApplied.velocity.z
      )
    )
  }
}
