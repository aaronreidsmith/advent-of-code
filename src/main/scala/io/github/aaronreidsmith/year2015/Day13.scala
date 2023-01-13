package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day13 extends Solution {
  type I = List[Person]
  type O1 = Int
  type O2 = Int

  case class Person(name: String, rules: Map[String, Int])

  override def parseInput(file: Source): List[Person] = {
    val gain = """^(.*?) would gain (\d+) happiness units by sitting next to (.*?)\.$""".r
    val lose = """^(.*?) would lose (\d+) happiness units by sitting next to (.*?)\.$""".r

    file
      .getLines()
      .foldLeft(Map.empty[String, Person]) { (acc, line) =>
        line match {
          case gain(name, value, neighbor) =>
            acc.get(name) match {
              case Some(person) => acc.updated(name, person.copy(rules = person.rules.updated(neighbor, value.toInt)))
              case None         => acc.updated(name, Person(name, Map(neighbor -> value.toInt)))
            }
          case lose(name, value, neighbor) =>
            acc.get(name) match {
              case Some(person) => acc.updated(name, person.copy(rules = person.rules.updated(neighbor, -value.toInt)))
              case None         => acc.updated(name, Person(name, Map(neighbor -> -value.toInt)))
            }
          case _ => acc
        }
      }
      .values
      .toList
  }

  override def part1(rules: List[Person]): Int = solution(rules)
  override def part2(rules: List[Person]): Int = {
    val updatedRules = rules.map(person => person.copy(rules = person.rules.updated("Aaron", 0)))
    val me           = Person("Aaron", rules.map(_.name -> 0).toMap)
    solution(me :: updatedRules)
  }

  private def solution(rules: List[Person]): Int = rules.permutations.foldLeft(0) { (currentMax, permutation) =>
    val maxIndex = permutation.length - 1
    val candidate = permutation.zipWithIndex.foldLeft(0) {
      case (acc, (person, index)) =>
        val leftNeighbor  = permutation(if (index == 0) maxIndex else index - 1)
        val rightNeighbor = permutation(if (index == maxIndex) 0 else index + 1)
        acc + person.rules.getOrElse(leftNeighbor.name, 0) + person.rules.getOrElse(rightNeighbor.name, 0)
    }
    currentMax.max(candidate)
  }
}
