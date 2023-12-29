package io.github.aaronreidsmith.year2023

import com.microsoft.z3.{BoolExpr, Context, RealExpr, Status}
import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day24 extends Solution {
  type I  = List[Hailstone]
  type O1 = Int
  type O2 = Long

  extension (d: Double) {
    def approxEqual(other: Double): Boolean = {
      val epsilon = 1e-9
      math.abs(d - other) < epsilon
    }
  }

  enum Intersection {
    case Identical
    case Parallel
    case Point(inFuture: Boolean, x: Double, y: Double)
  }

  case class Hailstone(x: Long, y: Long, z: Long, dx: Int, dy: Int, dz: Int) {
    def intersect(other: Hailstone): Intersection = {
      val a1 = this.dy.toDouble / this.dx.toDouble
      val b1 = this.y - a1 * this.x
      val a2 = other.dy.toDouble / other.dx.toDouble
      val b2 = other.y - a2 * other.x
      if (a1.approxEqual(a2)) {
        if (b1.approxEqual(b2)) {
          Intersection.Identical
        } else {
          Intersection.Parallel
        }
      } else {
        val cx       = (b2 - b1) / (a1 - a2)
        val cy       = cx * a1 + b1
        val inFuture = (cx > this.x) == (this.dx > 0) && (cx > other.x) == (other.dx > 0)
        Intersection.Point(inFuture, cx, cy)
      }
    }
  }

  override def parseInput(file: Source): List[Hailstone] = {
    val regex = """^(\d+), (\d+), (\d+) @ (-?\d+),\s+(-?\d+),\s+(-?\d+)$""".r
    file.getLines().toList.collect {
      case regex(x, y, z, dx, dy, dz) => Hailstone(x.toLong, y.toLong, z.toLong, dx.toInt, dy.toInt, dz.toInt)
    }
  }

  // Adapted from https://github.com/admp/aoc-2023/blob/467f4980ed578f020b5f680259d44ec0d54b6c2a/24/p1.py
  override def part1(input: List[Hailstone]): Int = {
    val minCoordinate = if (isTest) 7d else 200000000000000d
    val maxCoordinate = if (isTest) 27d else 400000000000000d
    input.combinations(2).count {
      case List(a, b) =>
        a.intersect(b) match {
          case Intersection.Identical => true
          case Intersection.Parallel  => false
          case Intersection.Point(inFuture, x, y) =>
            inFuture && minCoordinate <= x && x <= maxCoordinate && minCoordinate <= y && y <= maxCoordinate
        }
      case _ => false
    }
  }

  // Adapted from https://github.com/JeeZeh/advent-of-code/blob/4ed51ed865ace1c818dc7e7eca9142cf03981802/2023/java/src/main/java/day24/Solution.java#L90-L135
  override def part2(input: List[Hailstone]): Long = {
    val context = new Context

    def constrainPosition(
        fromX: RealExpr,
        fromVelX: RealExpr,
        toX: Long,
        toVelX: Long,
        time: RealExpr
    ): BoolExpr = {
      val throwX = context.mkAdd(fromX, context.mkMul(fromVelX, time))
      val hailX  = context.mkAdd(context.mkMul(time, context.mkReal(toVelX)), context.mkReal(toX))
      context.mkEq(hailX, throwX)
    }

    val x = context.mkRealConst("x")
    val y = context.mkRealConst("y")
    val z = context.mkRealConst("z")

    val vx = context.mkRealConst("vx")
    val vy = context.mkRealConst("vy")
    val vz = context.mkRealConst("vz")

    val solver = context.mkSolver()

    input.zipWithIndex.foreach { (hail, i) =>
      val time = context.mkRealConst(s"t$i")
      solver.add(context.mkGe(time, context.mkReal(0)))
      solver.add(constrainPosition(x, vx, hail.x, hail.dx, time))
      solver.add(constrainPosition(y, vy, hail.y, hail.dy, time))
      solver.add(constrainPosition(z, vz, hail.z, hail.dz, time))
    }

    if (solver.check() == Status.SATISFIABLE) {
      val model = solver.getModel
      List(x, y, z).foldLeft(0L)((acc, expr) => acc + model.eval(expr, true).toString.toLong)
    } else {
      -1
    }
  }
}
