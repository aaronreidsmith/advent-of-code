package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day02 extends Solution {
  type I  = List[List[Char]]
  type O1 = String
  type O2 = String

  enum Character(val value: Char) {
    case One   extends Character('1')
    case Two   extends Character('2')
    case Three extends Character('3')
    case Four  extends Character('4')
    case Five  extends Character('5')
    case Six   extends Character('6')
    case Seven extends Character('7')
    case Eight extends Character('8')
    case Nine  extends Character('9')
    case A     extends Character('A')
    case B     extends Character('B')
    case C     extends Character('C')
    case D     extends Character('D')

    def partOneLeft: Character = this match {
      case Two   => One
      case Three => Two
      case Five  => Four
      case Six   => Five
      case Eight => Seven
      case Nine  => Eight
      case _     => this
    }

    def partOneRight: Character = this match {
      case One   => Two
      case Two   => Three
      case Four  => Five
      case Five  => Six
      case Seven => Eight
      case Eight => Nine
      case _     => this
    }

    def partOneUp: Character = this match {
      case Four  => One
      case Five  => Two
      case Six   => Three
      case Seven => Four
      case Eight => Five
      case Nine  => Six
      case _     => this
    }

    def partOneDown: Character = this match {
      case One   => Four
      case Two   => Five
      case Three => Six
      case Four  => Seven
      case Five  => Eight
      case Six   => Nine
      case _     => this
    }

    def partTwoLeft: Character = this match {
      case Three => Two
      case Four  => Three
      case Six   => Five
      case Seven => Six
      case Eight => Seven
      case Nine  => Eight
      case B     => A
      case C     => B
      case _     => this
    }

    def partTwoRight: Character = this match {
      case Two   => Three
      case Three => Four
      case Five  => Six
      case Six   => Seven
      case Seven => Eight
      case Eight => Nine
      case A     => B
      case B     => C
      case _     => this
    }

    def partTwoUp: Character = this match {
      case Three => One
      case Six   => Two
      case Seven => Three
      case Eight => Four
      case A     => Six
      case B     => Seven
      case C     => Eight
      case D     => B
      case _     => this
    }

    def partTwoDown: Character = this match {
      case One   => Three
      case Two   => Six
      case Three => Seven
      case Four  => Eight
      case Six   => A
      case Seven => B
      case Eight => C
      case B     => D
      case _     => this
    }
  }

  override def parseInput(file: Source): List[List[Char]] = file.getLines().toList.map(_.toList)

  override def part1(input: List[List[Char]]): String = {
    val output = new StringBuilder
    @tailrec
    def helper(instructions: List[List[Char]], currentNum: Character = Character.Five): String = {
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

  override def part2(input: List[List[Char]]): String = {
    val output = new StringBuilder
    @tailrec
    def helper(instructions: List[List[Char]], currentNum: Character = Character.Five): String = {
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
