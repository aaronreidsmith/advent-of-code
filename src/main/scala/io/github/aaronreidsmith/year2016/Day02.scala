package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Solution, using}

import scala.annotation.tailrec
import scala.io.Source

object Day02 extends Solution {
  type I  = List[List[Char]]
  type O1 = String
  type O2 = String

  sealed trait Character {
    val value: Char
    // Default to not moving
    val partOneLeft: Character  = this
    val partOneRight: Character = this
    val partOneUp: Character    = this
    val partOneDown: Character  = this
    val partTwoLeft: Character  = this
    val partTwoRight: Character = this
    val partTwoUp: Character    = this
    val partTwoDown: Character  = this
  }
  case object One extends Character {
    val value: Char                      = '1'
    override val partOneRight: Character = Two
    override val partOneDown: Character  = Four
    override val partTwoDown: Character  = Three
  }
  case object Two extends Character {
    val value: Char                      = '2'
    override val partOneLeft: Character  = One
    override val partOneRight: Character = Three
    override val partOneDown: Character  = Five
    override val partTwoRight: Character = Three
    override val partTwoDown: Character  = Six
  }
  case object Three extends Character {
    val value: Char                      = '3'
    override val partOneLeft: Character  = Two
    override val partOneDown: Character  = Six
    override val partTwoLeft: Character  = Two
    override val partTwoRight: Character = Four
    override val partTwoUp: Character    = One
    override val partTwoDown: Character  = Seven
  }
  case object Four extends Character {
    val value: Char                      = '4'
    override val partOneRight: Character = Five
    override val partOneUp: Character    = One
    override val partOneDown: Character  = Seven
    override val partTwoLeft: Character  = Three
    override val partTwoDown: Character  = Eight
  }
  case object Five extends Character {
    val value: Char                      = '5'
    override val partOneLeft: Character  = Four
    override val partOneRight: Character = Six
    override val partOneUp: Character    = Two
    override val partOneDown: Character  = Eight
    override val partTwoRight: Character = Six
  }
  case object Six extends Character {
    val value: Char                      = '6'
    override val partOneLeft: Character  = Five
    override val partOneUp: Character    = Three
    override val partOneDown: Character  = Nine
    override val partTwoLeft: Character  = Five
    override val partTwoRight: Character = Seven
    override val partTwoUp: Character    = Two
    override val partTwoDown: Character  = A
  }
  case object Seven extends Character {
    val value: Char                      = '7'
    override val partOneRight: Character = Eight
    override val partOneUp: Character    = Four
    override val partTwoLeft: Character  = Six
    override val partTwoRight: Character = Eight
    override val partTwoUp: Character    = Three
    override val partTwoDown: Character  = B
  }
  case object Eight extends Character {
    val value: Char                      = '8'
    override val partOneLeft: Character  = Seven
    override val partOneRight: Character = Nine
    override val partOneUp: Character    = Five
    override val partTwoLeft: Character  = Seven
    override val partTwoRight: Character = Nine
    override val partTwoUp: Character    = Four
    override val partTwoDown: Character  = C
  }
  case object Nine extends Character {
    val value: Char                     = '9'
    override val partOneLeft: Character = Eight
    override val partOneUp: Character   = Six
    override val partTwoLeft: Character = Eight
  }
  case object A extends Character {
    val value: Char                      = 'A'
    override val partTwoRight: Character = B
    override val partTwoUp: Character    = Six
  }
  case object B extends Character {
    val value: Char                      = 'B'
    override val partTwoLeft: Character  = A
    override val partTwoRight: Character = C
    override val partTwoUp: Character    = Seven
    override val partTwoDown: Character  = D
  }
  case object C extends Character {
    val value: Char                     = 'C'
    override val partTwoLeft: Character = B
    override val partTwoUp: Character   = Eight
  }
  case object D extends Character {
    val value: Char                   = 'D'
    override val partTwoUp: Character = B
  }

  def run(): Unit = {
    println("Year 2016, Day 2")
    val input = using("2016/day02.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def parseInput(file: Source): List[List[Char]] = file.getLines().toList.map(_.toList)

  override protected[year2016] def part1(input: List[List[Char]]): String = {
    val output = new StringBuilder
    @tailrec
    def helper(instructions: List[List[Char]], currentNum: Character = Five): String = {
      if (instructions.isEmpty) {
        output.mkString
      } else {
        instructions.head match {
          case Nil =>
            output.append(currentNum.value)
            helper(instructions.tail, currentNum)
          case head :: tail =>
            val newCurrentNum = head match {
              case 'U' => currentNum.partOneUp
              case 'D' => currentNum.partOneDown
              case 'L' => currentNum.partOneLeft
              case 'R' => currentNum.partOneRight
              case _   => throw new IllegalArgumentException
            }
            helper(tail :: instructions.tail, newCurrentNum)
        }
      }
    }

    helper(input)
  }

  override protected[year2016] def part2(input: List[List[Char]]): String = {
    val output = new StringBuilder
    @tailrec
    def helper(instructions: List[List[Char]], currentNum: Character = Five): String = {
      if (instructions.isEmpty) {
        output.mkString
      } else {
        instructions.head match {
          case Nil =>
            output.append(currentNum.value)
            helper(instructions.tail, currentNum)
          case head :: tail =>
            val newCurrentNum = head match {
              case 'U' => currentNum.partTwoUp
              case 'D' => currentNum.partTwoDown
              case 'L' => currentNum.partTwoLeft
              case 'R' => currentNum.partTwoRight
              case _   => throw new IllegalArgumentException
            }
            helper(tail :: instructions.tail, newCurrentNum)
        }
      }
    }

    helper(input)
  }
}
