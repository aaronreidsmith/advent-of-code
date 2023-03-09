package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Long

  private val mappings = Map(
    '(' -> ')',
    ')' -> '(',
    '[' -> ']',
    ']' -> '[',
    '{' -> '}',
    '}' -> '{',
    '<' -> '>',
    '>' -> '<'
  )
  private val openings = Set('(', '[', '{', '<')

  override def parseInput(file: Source): List[String] = file.getLines().toList

  override def part1(input: List[String]): Int = {
    val scoreMap = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

    @tailrec
    def helper(instructions: String, stack: List[Char] = Nil): Int = instructions.headOption match {
      case Some(char) if openings.contains(char) => helper(instructions.tail, char :: stack) // Valid instruction
      case Some(closingChar) =>
        stack match {
          case head :: tail if mappings(closingChar) == head => helper(instructions.tail, tail) // Valid instruction
          case _                                             => scoreMap(closingChar)           // Corrupted instruction
        }
      // Incomplete instruction, ignore
      case None => 0
    }

    input.foldLeft(0)(_ + helper(_))
  }

  override def part2(input: List[String]): Long = {
    val scoreMap = Map(')' -> 1L, ']' -> 2L, '}' -> 3L, '>' -> 4L)

    @tailrec
    def helper(instructions: String, stack: List[Char] = Nil): String = instructions.headOption match {
      case Some(char) if openings.contains(char) => helper(instructions.tail, char :: stack) // Valid instruction
      case Some(closingChar) =>
        stack match {
          case head :: tail if mappings(closingChar) == head => helper(instructions.tail, tail) // Valid instruction
          case _                                             => ""                              // Corrupted instruction
        }
      // Incomplete instruction, calculate the rest of the line
      case None => stack.map(openingChar => mappings(openingChar)).mkString
    }

    val scores = input
      .map(instruction => helper(instruction))
      .collect {
        case remaining if remaining.nonEmpty => remaining.foldLeft(0L)((acc, char) => acc * 5 + scoreMap(char))
      }
      .sorted

    scores(scores.size / 2) // Take the middle score
  }
}
