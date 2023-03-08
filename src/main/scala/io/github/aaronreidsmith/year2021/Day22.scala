package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import scala.collection.immutable.NumericRange
import scala.io.Source

@Slow(part1 = true)
object Day22 extends Solution {
  type I  = List[Cube]
  type O1 = Long
  type O2 = Long

  sealed trait State
  case object On        extends State
  case object Off       extends State
  case object Undefined extends State // Used when merging cubes
  object State {
    def apply(str: String): State = str match {
      case "on"  => On
      case "off" => Off
      case _     => throw new IllegalArgumentException
    }
  }

  case class Cube(
      state: State,
      xRange: NumericRange[Long],
      yRange: NumericRange[Long],
      zRange: NumericRange[Long]
  ) {
    def intersect(other: Cube): Option[Cube] = for {
      actualXStart <- Some(this.xRange.start.max(other.xRange.start))
      actualXEnd   <- Some(this.xRange.end.min(other.xRange.end))
      if actualXStart <= actualXEnd
      actualYStart <- Some(this.yRange.start.max(other.yRange.start))
      actualYEnd   <- Some(this.yRange.end.min(other.yRange.end))
      if actualYStart <= actualYEnd
      actualZStart <- Some(this.zRange.start.max(other.zRange.start))
      actualZEnd   <- Some(this.zRange.end.min(other.zRange.end))
      if actualZStart <= actualZEnd
    } yield Cube(
      Undefined,
      NumericRange.inclusive(actualXStart, actualXEnd, 1),
      NumericRange.inclusive(actualYStart, actualYEnd, 1),
      NumericRange.inclusive(actualZStart, actualZEnd, 1)
    )

    def volume: Long = {
      (xRange.end - xRange.start + 1) * (yRange.end - yRange.start + 1) * (zRange.end - zRange.start + 1)
    }
  }

  object Cube {
    private val step = """^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)""".r
    def from(str: String): Cube = str match {
      case step(state, xStart, xEnd, yStart, yEnd, zStart, zEnd) =>
        Cube(
          State(state),
          NumericRange.inclusive(xStart.toLong, xEnd.toLong, 1),
          NumericRange.inclusive(yStart.toLong, yEnd.toLong, 1),
          NumericRange.inclusive(zStart.toLong, zEnd.toLong, 1)
        )
      case _ => throw new IllegalArgumentException
    }
  }

  override def parseInput(file: Source): List[Cube] = file.getLines().toList.map(Cube.from)

  override def part1(input: List[Cube]): Long = {
    input
      .foldLeft(Set.empty[(Long, Long, Long)]) { (acc, instruction) =>
        val updated = for {
          x <- instruction.xRange if -50 <= x && x <= 50
          y <- instruction.yRange if -50 <= y && y <= 50
          z <- instruction.zRange if -50 <= z && z <= 50
        } yield (x, y, z)

        instruction.state match {
          case On  => acc ++ updated
          case Off => acc -- updated
          case _   => acc
        }
      }
      .size
  }

  override def part2(input: List[Cube]): Long = {
    def count(cubes: List[Cube]): Long = cubes match {
      case Nil => 0
      case head :: tail =>
        head.state match {
          case Off => count(tail)
          case _ =>
            val minus = tail.foldLeft(Set.empty[Cube]) { (acc, cube) =>
              val intersection = head.intersect(cube).map(Set(_)).getOrElse(Set())
              acc ++ intersection
            }
            head.volume + count(tail) - count(minus.toList)
        }
    }

    count(input)
  }
}
