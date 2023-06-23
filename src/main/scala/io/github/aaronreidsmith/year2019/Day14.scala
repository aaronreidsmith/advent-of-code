package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Integral.Implicits.*

object Day14 extends Solution {
  type Reactions = Map[String, (Long, Map[String, Long])]
  type I         = Reactions
  type O1        = Long
  type O2        = Long

  // Adapted from https://www.reddit.com/r/adventofcode/comments/eafj32/comment/far2irk
  override def parseInput(file: Source): Reactions = {
    val chemical = """(\d+) ([A-Z]+)""".r
    file
      .getLines()
      .map { line =>
        val chemicals      = chemical.findAllMatchIn(line).map(m => m.group(2) -> m.group(1).toLong).toSeq
        val inputChemicals = chemicals.init.toMap
        val (outputChem, outputChemAmount) = chemicals.last
        (outputChem, (outputChemAmount, inputChemicals))
      }
      .toMap
  }

  override def part1(reactions: Reactions): Long = solution(reactions, 1)

  // Adapted from https://www.reddit.com/r/adventofcode/comments/eafj32/comment/favii5d
  override def part2(reactions: Map[String, (Long, Map[String, Long])]): Long = {
    val oneTrillion = 1000000000000L

    @tailrec
    def helper(fuel: Long, ore: Long): Long = if (ore > oneTrillion) {
      fuel
    } else {
      val guess   = (fuel + 1) * oneTrillion / ore
      val newFuel = guess.max(fuel + 1)
      val newOre  = solution(reactions, newFuel + 1)
      helper(newFuel, newOre)
    }

    val initialFuel = 1L
    val initialOre  = solution(reactions, initialFuel + 1)
    helper(initialFuel, initialOre)
  }

  private def solution(reactions: Reactions, fuelAmount: Long): Long = {
    def helper(
        chemical: String,
        amount: Long,
        excess: Map[String, Long] = Map.empty[String, Long].withDefaultValue(0L)
    ): (Long, Map[String, Long]) = chemical match {
      case "ORE" => (amount, excess)
      case _ =>
        val amountWithoutExcess = 0L.max(amount - excess(chemical))
        val amountFromExcess    = amount - amountWithoutExcess
        val excessWithoutAmount = excess + (chemical -> (excess(chemical) - amountFromExcess))

        val (outputAmount, inputChemicals) = reactions(chemical)
        val (reactionRepeat, outputExcess) = amountWithoutExcess /% outputAmount match {
          case (q, 0) => (q, 0L)
          case (q, r) => (q + 1, outputAmount - r)
        }

        val (ore, inputExcess) = inputChemicals.foldLeft((0L, excessWithoutAmount)) {
          case ((ore, excess), (inputChemical, inputAmount)) =>
            val (inputOre, inputExcess) = helper(inputChemical, reactionRepeat * inputAmount, excess)
            (ore + inputOre, inputExcess)
        }

        (ore, inputExcess + (chemical -> (inputExcess(chemical) + outputExcess)))
    }

    helper("FUEL", fuelAmount)._1
  }
}
