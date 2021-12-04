package io.github.aaronreidsmith.year2018

object Day22 {
  private sealed trait RegionType
  private case object Rocky  extends RegionType
  private case object Wet    extends RegionType
  private case object Narrow extends RegionType

  private sealed trait Tool
  private case object ClimbingGear extends Tool
  private case object Torch        extends Tool
  private case object Neither      extends Tool

  // Hardcoded rather than parsing input
  private val DEPTH  = 510
  private val TARGET = (10, 10)
  private val ALL_REGIONS = {
    for {
      x <- 0 to TARGET._1
      y <- 0 to DEPTH
    } yield (x, y) -> Region(x, y)
  }.toMap

  private case class Region(x: Int, y: Int) {
    lazy val geologicalIndex: Int = (x, y) match {
      case (0, 0) | TARGET => 0
      case (_, 0)          => x * 16807
      case (0, _)          => y * 48271
      case (_, _)          => ALL_REGIONS((x - 1, y)).erosionLevel * ALL_REGIONS((x, y - 1)).erosionLevel
    }

    lazy val erosionLevel: Int = (geologicalIndex + DEPTH) % 20183
    lazy val riskLevel: Int    = erosionLevel              % 3

    lazy val regionType: RegionType = riskLevel match {
      case 0 => Rocky
      case 1 => Wet
      case 2 => Narrow
      case _ => throw new IllegalArgumentException
    }
  }

  def main(args: Array[String]): Unit = {
    val (targetX, targetY) = TARGET
    val part1 = {
      for {
        x <- 0 to targetX
        y <- 0 to targetY
      } yield ALL_REGIONS((x, y)).riskLevel
    }.sum
    println(s"Part 1: $part1")
    println(s"Part 2: ${part2().min}")
  }

  // This is slowest thing I have ever written in my life, but it works 🤷
  private def part2(position: (Int, Int) = (0, 0), tool: Tool = Torch, minutes: Int = 0): Seq[Int] =
    if (position == TARGET) {
      Seq(minutes)
    } else {
      val currentRegion        = ALL_REGIONS(position).regionType
      val (currentX, currentY) = position
      // Assume we only move right and down
      val candidatePositions = Seq((currentX + 1, currentY), (currentX, currentY + 1)).filter {
        case (x, y) => x <= DEPTH && y <= TARGET._2
      }
      candidatePositions.flatMap { newPosition =>
        (currentRegion, ALL_REGIONS(newPosition).regionType) match {
          case (Rocky, Rocky) => part2(newPosition, tool, minutes + 1)
          case (Rocky, Wet) =>
            tool match {
              case ClimbingGear => part2(newPosition, tool, minutes + 1)
              case _            => part2(position, Neither, minutes + 7)
            }
          case (Rocky, Narrow) =>
            tool match {
              case Torch => part2(newPosition, tool, minutes + 1)
              case _     => part2(position, Neither, minutes + 7)
            }
          case (Wet, Rocky) =>
            tool match {
              case ClimbingGear => part2(newPosition, tool, minutes + 1)
              case _            => part2(position, Torch, minutes + 7)
            }
          case (Wet, Wet) => part2(newPosition, tool, minutes + 1)
          case (Wet, Narrow) =>
            tool match {
              case Neither => part2(newPosition, tool, minutes + 1)
              case _       => part2(position, Torch, minutes + 7)
            }
          case (Narrow, Rocky) =>
            tool match {
              case Torch => part2(newPosition, tool, minutes + 1)
              case _     => part2(position, ClimbingGear, minutes + 7)
            }
          case (Narrow, Wet) =>
            tool match {
              case Neither => part2(newPosition, tool, minutes + 1)
              case _       => part2(position, ClimbingGear, minutes + 7)
            }
          case (Narrow, Narrow) => part2(newPosition, tool, minutes + 1)
        }
      }
    }
}
