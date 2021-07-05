package io.github.aaronreidsmith.year2015

import scala.io.Source

object Day13 {
  private val gain = "^(.*?) would gain (\\d+) happiness units by sitting next to (.*?)\\.$".r
  private val lose = "^(.*?) would lose (\\d+) happiness units by sitting next to (.*?)\\.$".r

  protected[this] case class Person(name: String, rules: Map[String, Int])

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2015/day13.txt")
    val rules = input
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
          case _ => throw new IllegalArgumentException
        }
      }
      .values
      .toList
    input.close()

    println(s"Part 1: ${solution(rules)}")

    val updatedRules = rules.map(person => person.copy(rules = person.rules.updated("Aaron", 0)))
    val me           = Person("Aaron", rules.map(_.name -> 0).toMap)
    println(s"Part 2: ${solution(me :: updatedRules)}")
  }

  private def solution(rules: List[Person]): Int = rules.permutations.foldLeft(0) { (currentMax, permutation) =>
    val maxIndex = permutation.length - 1
    val candidate = permutation.zipWithIndex.foldLeft(0) {
      case (acc, (person, index)) =>
        val leftNeighbor  = permutation(if (index == 0) maxIndex else index - 1)
        val rightNeighbor = permutation(if (index == maxIndex) 0 else index + 1)
        acc + person.rules.getOrElse(leftNeighbor.name, 0) + person.rules.getOrElse(rightNeighbor.name, 0)
    }
    math.max(currentMax, candidate)
  }
}
