package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.using

import scala.io.Source

object Day15 {
  private val ingredient =
    "^(.*?): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)$".r

  private[year2015] case class Ingredient(
      name: String,
      capacity: Int,
      durability: Int,
      flavor: Int,
      texture: Int,
      calories: Int
  )

  private case class Cookie(ingredients: List[Ingredient]) {
    require(ingredients.length == 100)

    lazy val score: Int = {
      val (totalCapacity, totalDurability, totalFlavor, totalTexture) = ingredients.foldLeft((0, 0, 0, 0)) {
        case ((capAcc, durAcc, flavAcc, texAcc), ing) =>
          (capAcc + ing.capacity, durAcc + ing.durability, flavAcc + ing.flavor, texAcc + ing.texture)
      }
      if (Seq(totalCapacity, totalDurability, totalFlavor, totalTexture).exists(_ <= 0)) 0
      else totalCapacity * totalDurability * totalFlavor * totalTexture
    }

    lazy val calories: Int = ingredients.foldLeft(0)(_ + _.calories)
  }

  def main(args: Array[String]): Unit = {
    val choices = using("2015/day15.txt")(parseInput)
    println(s"Part 1: ${part1(choices)}")
    println(s"Part 2: ${part2(choices)}")
  }

  private[year2015] def parseInput(file: Source): List[Ingredient] = file.getLines().foldLeft(List.empty[Ingredient]) {
    case (acc, ingredient(name, capacity, durability, flavor, texture, calories)) =>
      Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt) :: acc
    case _ => throw new IllegalArgumentException
  }

  private[year2015] def part1(choices: List[Ingredient]): Int = {
    val ingredients = List.fill(100)(choices).flatten
    ingredients.combinations(100).foldLeft(0) { (currentBest, combination) =>
      val cookie = Cookie(combination)
      currentBest.max(cookie.score)
    }
  }

  private[year2015] def part2(choices: List[Ingredient]): Int = {
    val ingredients = List.fill(100)(choices).flatten
    ingredients.combinations(100).foldLeft(0) { (currentBest, combination) =>
      val cookie = Cookie(combination)
      if (cookie.calories == 500) currentBest.max(cookie.score) else currentBest
    }
  }
}
