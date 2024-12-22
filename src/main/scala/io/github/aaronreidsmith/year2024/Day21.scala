package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.extensions.*
import io.github.aaronreidsmith.{Direction, Point, Solution}

import scala.collection.mutable
import scala.io.Source

object Day21 extends Solution {
  type I  = List[String]
  type O1 = Long
  type O2 = Long

  private val keypad =
    """789
      |456
      |123
      | 0A
      | ^@
      |<v>""".stripMargin.toGrid.toList.map(_.swap).toMap

  override def parseInput(file: Source): List[String] = {
    file.getLines().toList
  }

  override def part1(input: List[String]): Long = solution(input, 2)
  override def part2(input: List[String]): Long = solution(input, 25)

  // TODO: Works for part 1, but not part 2
  private def solution(input: List[String], limit: Int): Long = {
    val cache = mutable.Map.empty[(String, Int, Int), Long]
    def helper(sequence: String, depth: Int): Long = {
      cache.getOrElseUpdate(
        (sequence, limit, depth), {
          val initKey = if (depth == 0) keypad('A') else keypad('@')
          sequence
            .foldLeft((initKey, 0L)) {
              case ((pos, sum), char) =>
                val next          = keypad(char)
                val Point(dx, dy) = next - pos
                val paths = (
                  (if (dx < 0) "^".repeat(-dx) else "v".repeat(dx)) +
                    (if (dy < 0) "<".repeat(-dy) else ">".repeat(dy))
                ).permutations
                  .filter { path =>
                    path
                      .scanLeft(pos)((point, dir) => point.move(Direction.fromChar(dir)))
                      .forall(keypad.values.toSet.contains)
                  }
                  .map(path => s"$path@")
                val toAdd = if (depth == limit) {
                  paths.map(_.length.toLong).min
                } else {
                  paths.map(helper(_, depth + 1)).min
                }
                (next, sum + toAdd)
            }
            ._2
        }
      )
    }

    input.foldLeft(0L)((acc, code) => acc + (helper(code, 0) * code.filter(_.isDigit).toLong))
  }

}
