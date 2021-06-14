package io.github.aaronreidsmith.year2018

import scala.annotation.tailrec

object Day14 {
  def main(args: Array[String]): Unit = {
    val endCondition      = 540391
    val endConditionRegex = endCondition.toString.r

    @tailrec
    def part1(recipes: Vector[Char] = Vector('3', '7'), pointer1: Int = 0, pointer2: Int = 1): String =
      if (recipes.length >= endCondition + 10) {
        recipes.mkString.slice(endCondition, endCondition + 10)
      } else {
        val elf1        = recipes(pointer1).asDigit
        val elf2        = recipes(pointer2).asDigit
        val newRecipe   = s"${elf1 + elf2}"
        val newRecipes  = recipes ++ newRecipe.toVector
        val newPointer1 = (elf1 + 1 + pointer1) % newRecipes.length
        val newPointer2 = (elf2 + 1 + pointer2) % newRecipes.length
        part1(newRecipes, newPointer1, newPointer2)
      }

    @tailrec
    def part2(
        recipes: Vector[Char] = Vector('3', '7'),
        pointer1: Int = 0,
        pointer2: Int = 1,
        mostRecent: String = ""
    ): Int =
      if (endConditionRegex.findFirstMatchIn(mostRecent).isDefined) {
        recipes.mkString.split(endCondition.toString).head.length
      } else {
        val elf1        = recipes(pointer1).asDigit
        val elf2        = recipes(pointer2).asDigit
        val newRecipe   = s"${elf1 + elf2}"
        val newRecipes  = recipes ++ newRecipe.toVector
        val newPointer1 = (elf1 + 1 + pointer1) % newRecipes.length
        val newPointer2 = (elf2 + 1 + pointer2) % newRecipes.length
        part2(newRecipes, newPointer1, newPointer2, newRecipes.takeRight(7).mkString)
      }

    println(s"Part 1: ${part1()}")
    println(s"Part 2: ${part2()}")
  }
}
