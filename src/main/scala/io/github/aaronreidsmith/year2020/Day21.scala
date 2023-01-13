package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day21 extends Solution {
  type I  = (List[Rule], Map[String, String], Set[String])
  type O1 = Int
  type O2 = String

  case class Rule(ingredients: Set[String], allergens: Set[String])

  override def parseInput(file: Source): (List[Rule], Map[String, String], Set[String]) = {
    val rule = """^(.*) \(contains (.*)\)$""".r
    val rules = file.getLines().toList.collect {
      case rule(ingredients, allergens) => Rule(ingredients.split(' ').toSet, allergens.split(", ").toSet)
    }
    val allAllergens = rules.foldLeft(Set.empty[String])(_ ++ _.allergens)

    @tailrec
    def helper(
        candidates: Map[String, Set[String]],
        mapping: Map[String, String] = Map.empty,
        mapped: Set[String] = Set.empty
    ): (Map[String, String], Set[String]) = if (mapping.size >= allAllergens.size) {
      (mapping, mapped)
    } else {
      val (mappedAllergen, mappedIngredient) = candidates.collectFirst {
        case (allergen, ingredients) if ingredients.size == 1 => (allergen, ingredients.head)
      }.get
      val updatedMapping    = mapping.updated(mappedAllergen, mappedIngredient)
      val updatedMapped     = mapped + mappedIngredient
      val updatedCandidates = candidates.removed(mappedAllergen).view.mapValues(_ -- updatedMapped).toMap
      helper(updatedCandidates, updatedMapping, updatedMapped)
    }

    val initialCandidates = allAllergens.foldLeft(Map.empty[String, Set[String]]) { (acc, allergen) =>
      val ingredients = rules
        .collect { case rule if rule.allergens.contains(allergen) => rule.ingredients }
        .reduceLeft(_ & _)
      acc + (allergen -> ingredients)
    }
    val (mapping, mapped) = helper(initialCandidates)

    (rules, mapping, mapped)
  }

  override def part1(input: (List[Rule], Map[String, String], Set[String])): Int = {
    val (rules, _, mapped) = input
    rules.foldLeft(0)((acc, rule) => acc + (rule.ingredients -- mapped).size)
  }

  override def part2(input: (List[Rule], Map[String, String], Set[String])): String = {
    val (_, mapping, _) = input
    mapping.toSeq.sortBy(_._1).map(_._2).mkString(",")
  }
}
