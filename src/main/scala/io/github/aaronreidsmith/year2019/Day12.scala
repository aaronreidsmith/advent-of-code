package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.util.FileUtils

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day12 extends FileUtils {
  private case class Point(x: Int = 0, y: Int = 0, z: Int = 0) {
    def +(other: Point): Point = this.copy(x = this.x + other.x, y = this.y + other.y, z = this.z + other.z)
    def compare(other: Point): Point = {
      val xChange = other.x.compare(this.x)
      val yChange = other.y.compare(this.y)
      val zChange = other.z.compare(this.z)
      Point(xChange, yChange, zChange)
    }
    def energy: Int = x.abs + y.abs + z.abs
  }
  private case class Moon(position: Point, velocity: Point = Point(0, 0, 0)) {
    def applyGravity(other: Moon): Moon = this.copy(velocity = this.velocity + this.position.compare(other.position))
    def applyVelocity: Moon             = this.copy(position = this.position + this.velocity)
    def kineticEnergy: Int              = velocity.energy
    def potentialEnergy: Int            = position.energy
    def totalEnergy: Int                = potentialEnergy * kineticEnergy
  }

  private val moonEntry = "^<x=\\s*(-?\\d+), y=\\s*(-?\\d+), z=\\s*(-?\\d+)>$".r("x", "y", "z")

  def main(args: Array[String]): Unit = {
    val moons = using(Source.fromResource("2019/day12.txt")) { file =>
      file.getLines().foldLeft(Vector.empty[Moon]) { (acc, line) =>
        line match {
          case moonEntry(x, y, z) => acc :+ Moon(Point(x.toInt, y.toInt, z.toInt))
          case other              => throw new IllegalArgumentException(s"'$other' is not a valid entry'")
        }
      }
    }
    println(s"Part 1: ${part1(moons)}")
    println(s"Part 2: ${part2(moons)}")
  }

  @tailrec
  private def part1(moons: Vector[Moon], iteration: Int = 0): Int = if (iteration >= 1000) {
    moons.foldLeft(0)(_ + _.totalEnergy)
  } else {
    part1(nextState(moons), iteration + 1)
  }

  // Adapted from https://git.io/J1O9W
  private def part2(moons: Vector[Moon]): Long = {
    @tailrec
    def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
    def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

    val baseFunction = cycleLength(moons) _
    val xCycle       = baseFunction(moon => (moon.position.x, moon.velocity.x))
    val yCycle       = baseFunction(moon => (moon.position.y, moon.velocity.y))
    val zCycle       = baseFunction(moon => (moon.position.z, moon.velocity.z))

    lcm(lcm(xCycle, yCycle), zCycle)
  }

  // Adapted from https://git.io/J1O9Z
  private def cycleLength(moons: Vector[Moon])(toTuple: Moon => (Int, Int)): Int = {
    val memo = mutable.Map.empty[Vector[(Int, Int)], Vector[(Int, Int)]]
    Iterator
      .iterate((moons, Vector())) { case (key, value) => (nextState(key), value) }
      .zipWithIndex
      .map { case ((key, value), index) => (index, memo.put(key.map(toTuple), value.map(toTuple))) }
      .dropWhile { case (_, maybeValue) => maybeValue.isEmpty }
      .next()
      ._1
  }

  // Lol, good luck figuring this out in a year+
  private def nextState(moons: Vector[Moon]): Vector[Moon] =
    moons.map(moons.foldLeft(_)(_.applyGravity(_)).applyVelocity)
}
