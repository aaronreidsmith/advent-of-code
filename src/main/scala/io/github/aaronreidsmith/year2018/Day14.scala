package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import scala.io.Source

@Slow(part2 = true)
object Day14 extends Solution {
  type I  = Int
  type O1 = String
  type O2 = Int

  case class State(
      recipes: Vector[Char] = Vector('3', '7'),
      pointer1: Int = 0,
      pointer2: Int = 1,
      mostRecent: String = ""
  ) {
    def next(take: Int): State = {
      val elf1        = recipes(pointer1).asDigit
      val elf2        = recipes(pointer2).asDigit
      val newRecipe   = s"${elf1 + elf2}"
      val newRecipes  = recipes ++ newRecipe.toVector
      val newPointer1 = (elf1 + 1 + pointer1) % newRecipes.length
      val newPointer2 = (elf2 + 1 + pointer2) % newRecipes.length
      State(newRecipes, newPointer1, newPointer2, newRecipes.takeRight(take).mkString)
    }
  }

  override def parseInput(file: Source): Int = file.mkString.trim.toInt

  override def part1(endCondition: Int): String = {
    val finalState = Iterator
      .iterate(State())(_.next(endCondition.toString.length))
      .find(state => state.recipes.length >= endCondition + 10)
      .get
    finalState.recipes.mkString.slice(endCondition, endCondition + 10)
  }

  override def part2(endCondition: Int): Int = {
    val endConditionString = endCondition.toString
    val endConditionRegex  = endConditionString.r
    val finalState = Iterator
      .iterate(State())(_.next(endConditionString.length))
      .find(state => endConditionRegex.findFirstMatchIn(state.mostRecent).isDefined)
      .get
    finalState.recipes.mkString.split(endCondition.toString).head.length
  }
}
