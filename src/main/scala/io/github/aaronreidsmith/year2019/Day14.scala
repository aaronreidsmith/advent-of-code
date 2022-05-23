package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.using

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Integral.Implicits._

object Day14 {
  def main(args: Array[String]): Unit = {
    val input = using("2019/day14.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  // Adapted from https://www.reddit.com/r/adventofcode/comments/eafj32/comment/far2irk
  private[year2019] def parseInput(file: Source): Map[String, (Long, Map[String, Long])] = {
    val chemical = "(\\d+) ([A-Z]+)".r
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

  private[year2019] def part1(reactions: Map[String, (Long, Map[String, Long])], fuelAmount: Long = 1): Long = {
    def helper(
        chemical: String,
        amount: Long,
        excess: Map[String, Long]
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
          case (acc, _) => acc
        }

        (ore, inputExcess + (chemical -> (inputExcess(chemical) + outputExcess)))
    }

    helper("FUEL", fuelAmount, Map.empty[String, Long].withDefaultValue(0L))._1
  }

  // Adapted from https://www.reddit.com/r/adventofcode/comments/eafj32/comment/favii5d
  private[year2019] def part2(reactions: Map[String, (Long, Map[String, Long])]): Long = {
    val oneTrillion = 1000000000000L

    @tailrec
    def helper(fuel: Long, ore: Long): Long = if (ore > oneTrillion) {
      fuel
    } else {
      val guess   = (fuel + 1) * oneTrillion / ore
      val newFuel = guess.max(fuel + 1)
      val newOre  = part1(reactions, newFuel + 1)
      helper(newFuel, newOre)
    }

    val initialFuel = 1L
    val initialOre  = part1(reactions, initialFuel + 1)
    helper(initialFuel, initialOre)
  }
}
