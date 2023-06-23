package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution
import org.apache.commons.math3.util.ArithmeticUtils

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day12 extends Solution {
  type I  = Vector[Moon]
  type O1 = Int
  type O2 = Long

  case class Point3(x: Int = 0, y: Int = 0, z: Int = 0) {
    infix def +(other: Point3): Point3 = this.copy(x = this.x + other.x, y = this.y + other.y, z = this.z + other.z)
    def compare(other: Point3): Point3 = {
      val xChange = other.x.compare(this.x)
      val yChange = other.y.compare(this.y)
      val zChange = other.z.compare(this.z)
      Point3(xChange, yChange, zChange)
    }
    def energy: Int = x.abs + y.abs + z.abs
  }

  case class Moon(position: Point3, velocity: Point3 = Point3()) {
    def applyGravity(other: Moon): Moon = this.copy(velocity = this.velocity + this.position.compare(other.position))
    def applyVelocity: Moon             = this.copy(position = this.position + this.velocity)
    def kineticEnergy: Int              = velocity.energy
    def potentialEnergy: Int            = position.energy
    def totalEnergy: Int                = potentialEnergy * kineticEnergy
  }

  override def parseInput(file: Source): Vector[Moon] = {
    val moon = """^<x=\s*(-?\d+), y=\s*(-?\d+), z=\s*(-?\d+)>$""".r
    file.getLines().toVector.collect {
      case moon(x, y, z) => Moon(Point3(x.toInt, y.toInt, z.toInt))
    }
  }

  override def part1(input: Vector[Moon]): Int = {
    val iterations = if (isTest) 10 else 1000

    @tailrec
    def helper(moons: Vector[Moon], iteration: Int = 0): Int = {
      if (iteration >= iterations) {
        moons.foldLeft(0)(_ + _.totalEnergy)
      } else {
        helper(nextState(moons), iteration + 1)
      }
    }

    helper(input)
  }

  // Adapted from https://git.io/J1O9W
  override def part2(moons: Vector[Moon]): Long = {
    // Doesn't work with ArithmeticUtils.lcm for some reason, so had to define my own
    def lcm(a: Long, b: Long): Long = a * b / ArithmeticUtils.gcd(a, b)

    // Adapted from https://git.io/J1O9Z
    def cycleLength(moons: Vector[Moon])(toTuple: Moon => (Int, Int)): Int = {
      val memo = mutable.Map.empty[Vector[(Int, Int)], Vector[(Int, Int)]]
      Iterator
        .iterate((moons, Vector()))((key, value) => (nextState(key), value))
        .zipWithIndex
        .map { case ((key, value), index) => (index, memo.put(key.map(toTuple), value.map(toTuple))) }
        .dropWhile((_, maybeValue) => maybeValue.isEmpty)
        .next()
        ._1
    }

    val baseFunction = cycleLength(moons) _
    val xCycle       = baseFunction(moon => (moon.position.x, moon.velocity.x))
    val yCycle       = baseFunction(moon => (moon.position.y, moon.velocity.y))
    val zCycle       = baseFunction(moon => (moon.position.z, moon.velocity.z))

    lcm(lcm(xCycle, yCycle), zCycle)
  }

  // Lol, good luck figuring this out in a year+
  private def nextState(moons: Vector[Moon]): Vector[Moon] = {
    moons.map(moons.foldLeft(_)(_.applyGravity(_)).applyVelocity)
  }
}
