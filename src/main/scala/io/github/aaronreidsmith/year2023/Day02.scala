package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day02 extends Solution {
  type I  = List[Game]
  type O1 = Int
  type O2 = Int

  case class Game(id: Int, outcomes: List[Outcome])
  case class Outcome(red: Int = 0, green: Int = 0, blue: Int = 0)

  override def parseInput(file: Source): List[Game] = file.getLines().toList.map { line =>
    val Array(gameNum, outcomesRaw, _*) = line.split(": "): @unchecked
    val id    = gameNum.filter(_.isDigit).toInt
    val outcomes = outcomesRaw.split("; ").toList.map { outcome =>
      outcome.split(", ").foldLeft(Outcome()) { (acc, pull) =>
        val Array(num, color, _*) = pull.split(' '): @unchecked
        color match {
          case "red"   => acc.copy(red = num.toInt)
          case "green" => acc.copy(green = num.toInt)
          case "blue"  => acc.copy(blue = num.toInt)
          case _       => acc
        }
      }
    }
    Game(id, outcomes)
  }
  
  override def part1(input: List[Game]): Int = input.foldLeft(0) { (acc, game) =>
    if (game.outcomes.forall(outcome => outcome.red <= 12 && outcome.green <= 13 && outcome.blue <= 14)) {
      acc + game.id
    } else {
      acc
    }
  }
  
  override def part2(input: List[Game]): Int = input.foldLeft(0) { (acc, game) =>
    val minRed   = game.outcomes.map(_.red).max
    val minGreen = game.outcomes.map(_.green).max
    val minBlue  = game.outcomes.map(_.blue).max
    acc + (minRed * minGreen * minBlue)
  }
}
