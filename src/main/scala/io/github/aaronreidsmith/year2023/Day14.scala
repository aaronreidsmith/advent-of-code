package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import scala.io.Source

// Adapted from https://topaz.github.io/paste/#XQAAAQCKAgAAAAAAAAAyGUj/TtFAzGBzqSakCRZKYNPHEk2cn5Nz7Nm2XkOH1l8Bqkw1xGuQ9zra+T7mcsZeGErjbYnUQgo5w+8WLsWcQtijEJxZ0yyBknYjjpa8aaW1FeW020/o/R+A6rHPZGZZ0JYzTFJgq96x0RJ8Z45PjQAGfZfSReaq3+H3jKbPJgrUKGfbpX/n9SOjiqhC3MxKwmbe9Ts1AT0X3pLl4vCqpFTM8SOUQa6P3Q11DaoL1yKNf1DxSWu23P7BLrmkfpsJju8qzx0zpbq8cB5kehBJkcIa4FSUPL9bACYLWSKGlnMKzjXaDoihrdryAA2Kowo2Ps7fPKllQ+Akccud2m8obZwCxaORnPow/enKAZjeJx9sQ6nfgIuipqxaxJeHCvR2K8DLzQMsvHC1/xrUYpfpblunQME3JhtXYgWjXLf+AL7aXqCloecF/WwWUpF6dIKDhli+zLIPJtj+pNfw
@Slow(part2 = true)
object Day14 extends Solution {
  type I  = Vector[String]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Vector[String] = rotate(file.getLines().toVector)

  override def part1(input: Vector[String]): Int = load(roll(input))

  override def part2(input: Vector[String]): Int = {
    def spin(state: Vector[String]): Vector[String] = rotate(roll(rotate(roll(rotate(roll(rotate(roll(state))))))))

    load((0 until 1000).foldLeft(input)((acc, _) => spin(acc)))
  }

  private def rotate(input: Vector[String]): Vector[String] = input.transpose.map(_.reverse.mkString)

  private def roll(input: Vector[String]): Vector[String] = input.map { line =>
    line.split('#').map(_.sorted.mkString("")).mkString("#").padTo(line.length, '#')
  }

  private def load(input: Vector[String]): Int = input.foldLeft(0) { (acc, line) =>
    acc + line.zipWithIndex.foldLeft(0) {
      case (innerAcc, (char, i)) if char == 'O' => innerAcc + i + 1
      case (innerAcc, _)                        => innerAcc
    }
  }
}
