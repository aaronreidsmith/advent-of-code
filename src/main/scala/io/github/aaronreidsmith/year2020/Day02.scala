package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day02 extends Solution {
  type I  = List[Entry]
  type O1 = Int
  type O2 = Int

  case class Entry(start: Int, end: Int, target: Char, password: String)

  override def parseInput(file: Source): List[Entry] = {
    val entry = """^(\d+)-(\d+) ([a-z]): ([a-z]+)$""".r
    file.getLines().toList.collect {
      case entry(start, end, target, password) => Entry(start.toInt, end.toInt, target.head, password)
    }
  }

  override def part1(input: List[Entry]): Int = input.count { entry =>
    val targetCount = entry.password.count(_ == entry.target)
    entry.start <= targetCount && targetCount <= entry.end
  }

  override def part2(input: List[Entry]): Int = input.count { entry =>
    entry.password(entry.start - 1) == entry.target ^ entry.password(entry.end - 1) == entry.target
  }
}
