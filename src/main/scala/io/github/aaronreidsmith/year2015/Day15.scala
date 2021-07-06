package io.github.aaronreidsmith.year2015

import scala.io.Source

object Day15 {
  private val ingredient =
    "^(.*?): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)$".r

  protected[this] case class Ingredient(
      name: String,
      capacity: Int,
      durability: Int,
      flavor: Int,
      texture: Int,
      calories: Int
  )

  protected[this] case class Cookie(ingredients: List[Ingredient]) {
    assert(ingredients.length == 100)

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
    val input = Source.fromResource("2015/day15.txt")
    val choices = input.getLines().foldLeft(List.empty[Ingredient]) {
      case (acc, ingredient(name, capacity, durability, flavor, texture, calories)) =>
        acc :+ Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
      case _ => throw new IllegalArgumentException
    }
    input.close()
    val ingredients = List.fill(100)(choices).flatten

    val part1 = ingredients.combinations(100).foldLeft(0) { (currentBest, combination) =>
      val cookie = Cookie(combination)
      math.max(currentBest, cookie.score)
    }
    println(s"Part 1: $part1")

    val part2 = ingredients.combinations(100).foldLeft(0) { (currentBest, combination) =>
      val cookie = Cookie(combination)
      if (cookie.calories == 500) math.max(currentBest, cookie.score) else currentBest
    }
    println(s"Part 2: $part2")
  }
}
