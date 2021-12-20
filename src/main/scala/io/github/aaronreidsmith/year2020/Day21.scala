package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

object Day21 {
  private val rule = "^(.*) \\(contains (.*)\\)$".r

  private case class Rule(ingredients: Set[String], allergens: Set[String])

  def main(args: Array[String]): Unit = {
    val rules = using("2020/day21.txt") { file =>
      file.getLines().toList.map {
        case rule(ingredients, allergens) => Rule(ingredients.split(' ').toSet, allergens.split(", ").toSet)
        case _                            => throw new IllegalArgumentException
      }
    }
    val allAllergens = rules.foldLeft(Set.empty[String])(_ ++ _.allergens)
    val initialCandidates = allAllergens.map { allergen =>
      val ingredients = rules
        .collect {
          case rule if rule.allergens.contains(allergen) => rule.ingredients
        }
        .reduceLeft(_ & _)
      allergen -> ingredients
    }.toMap

    @tailrec
    def solution(
        candidates: Map[String, Set[String]],
        mapping: Map[String, String] = Map(),
        mapped: Set[String] = Set()
    ): (Map[String, String], Set[String]) = if (mapping.size >= allAllergens.size) {
      (mapping, mapped)
    } else {
      val (mappedAllergen, mappedIngredient) = candidates.collectFirst {
        case (allergen, ingredients) if ingredients.size == 1 => (allergen, ingredients.head)
      }.get
      val updatedMapping    = mapping.updated(mappedAllergen, mappedIngredient)
      val updatedMapped     = mapped + mappedIngredient
      val updatedCandidates = candidates.removed(mappedAllergen).view.mapValues(_ -- updatedMapped).toMap
      solution(updatedCandidates, updatedMapping, updatedMapped)
    }

    val (mapping, mapped) = solution(initialCandidates)
    val part1             = rules.foldLeft(0)((acc, rule) => acc + (rule.ingredients -- mapped).size)
    println(s"Part 1: $part1")
    val part2 = mapping.toSeq.sortBy(_._1).map(_._2).mkString(",")
    println(s"Part 2: $part2")
  }
}
