package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec
import scala.io.Source

object Day02 {
  sealed trait Character {
    val value: String
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
    override val value: String           = "1"
    override val partOneRight: Character = Two
    override val partOneDown: Character  = Four
    override val partTwoDown: Character  = Three
  }
  case object Two extends Character {
    override val value: String           = "2"
    override val partOneLeft: Character  = One
    override val partOneRight: Character = Three
    override val partOneDown: Character  = Five
    override val partTwoRight: Character = Three
    override val partTwoDown: Character  = Six
  }
  case object Three extends Character {
    override val value: String           = "3"
    override val partOneLeft: Character  = Two
    override val partOneDown: Character  = Six
    override val partTwoLeft: Character  = Two
    override val partTwoRight: Character = Four
    override val partTwoUp: Character    = One
    override val partTwoDown: Character  = Seven
  }
  case object Four extends Character {
    override val value: String           = "4"
    override val partOneRight: Character = Five
    override val partOneUp: Character    = One
    override val partOneDown: Character  = Seven
    override val partTwoLeft: Character  = Three
    override val partTwoDown: Character  = Eight
  }
  case object Five extends Character {
    override val value: String           = "5"
    override val partOneLeft: Character  = Four
    override val partOneRight: Character = Six
    override val partOneUp: Character    = Two
    override val partOneDown: Character  = Eight
    override val partTwoRight: Character = Six
  }
  case object Six extends Character {
    override val value: String           = "6"
    override val partOneLeft: Character  = Five
    override val partOneUp: Character    = Three
    override val partOneDown: Character  = Nine
    override val partTwoLeft: Character  = Five
    override val partTwoRight: Character = Seven
    override val partTwoUp: Character    = Two
    override val partTwoDown: Character  = A
  }
  case object Seven extends Character {
    override val value: String           = "7"
    override val partOneRight: Character = Eight
    override val partOneUp: Character    = Four
    override val partTwoLeft: Character  = Six
    override val partTwoRight: Character = Eight
    override val partTwoUp: Character    = Three
    override val partTwoDown: Character  = B
  }
  case object Eight extends Character {
    override val value: String           = "8"
    override val partOneLeft: Character  = Seven
    override val partOneRight: Character = Nine
    override val partOneUp: Character    = Five
    override val partTwoLeft: Character  = Seven
    override val partTwoRight: Character = Nine
    override val partTwoUp: Character    = Four
    override val partTwoDown: Character  = C
  }
  case object Nine extends Character {
    override val value: String          = "9"
    override val partOneLeft: Character = Eight
    override val partOneUp: Character   = Six
    override val partTwoLeft: Character = Eight
  }
  case object A extends Character {
    override val value: String           = "A"
    override val partTwoRight: Character = B
    override val partTwoUp: Character    = Six
  }
  case object B extends Character {
    override val value: String           = "B"
    override val partTwoLeft: Character  = A
    override val partTwoRight: Character = C
    override val partTwoUp: Character    = Seven
    override val partTwoDown: Character  = D
  }
  case object C extends Character {
    override val value: String          = "C"
    override val partTwoLeft: Character = B
    override val partTwoUp: Character   = Eight
  }
  case object D extends Character {
    override val value: String        = "D"
    override val partTwoUp: Character = B
  }

  def main(args: Array[String]): Unit = {
    val input        = Source.fromResource("2016/day02.txt")
    val instructions = input.getLines().toList.map(_.toCharArray.toList)
    input.close()

    println(s"Part 1: ${part1(instructions)}")
    println(s"Part 2: ${part2(instructions)}")
  }

  @tailrec
  private def part1(instructions: List[List[Char]], currentCode: String = "", currentNum: Character = Five): String =
    if (instructions.isEmpty) {
      currentCode
    } else {
      instructions.head match {
        case Nil => part1(instructions.tail, currentCode + currentNum.value, currentNum)
        case head :: tail =>
          val newCurrentNum = head match {
            case 'U' => currentNum.partOneUp
            case 'D' => currentNum.partOneDown
            case 'L' => currentNum.partOneLeft
            case 'R' => currentNum.partOneRight
            case _   => throw new IllegalArgumentException
          }
          part1(tail :: instructions.tail, currentCode, newCurrentNum)
      }
    }

  @tailrec
  private def part2(instructions: List[List[Char]], currentCode: String = "", currentNum: Character = Five): String =
    if (instructions.isEmpty) {
      currentCode
    } else {
      instructions.head match {
        case Nil => part2(instructions.tail, currentCode + currentNum.value, currentNum)
        case head :: tail =>
          val newCurrentNum = head match {
            case 'U' => currentNum.partTwoUp
            case 'D' => currentNum.partTwoDown
            case 'L' => currentNum.partTwoLeft
            case 'R' => currentNum.partTwoRight
            case _   => throw new IllegalArgumentException
          }
          part2(tail :: instructions.tail, currentCode, newCurrentNum)
      }
    }
}
