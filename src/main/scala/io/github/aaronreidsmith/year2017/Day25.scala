package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day25 extends Solution {
  type I  = (Char, Int, Map[Char, State])
  type O1 = Int
  type O2 = Nothing

  sealed trait State {
    def nextValue(current: Int): Int
    def positionDelta(current: Int): Int
    def nextState(current: Int): Char
  }

  override def parseInput(file: Source): (Char, Int, Map[Char, State]) = {
    def delta(direction: String): Int = if (direction == "left") -1 else 1

    val header = """Begin in state ([A-Z]).
                   |Perform a diagnostic checksum after (\d+) steps.""".stripMargin.r
    val state = """In state ([A-Z]):
                  |  If the current value is 0:
                  |    - Write the value (\d).
                  |    - Move one slot to the (left|right).
                  |    - Continue with state ([A-Z]).
                  |  If the current value is 1:
                  |    - Write the value (\d).
                  |    - Move one slot to the (left|right).
                  |    - Continue with state ([A-Z]).""".stripMargin.r

    val input                        = file.mkString.trim.split("\n\n")
    val header(startingState, steps) = input.head
    val states = input.tail.foldLeft(Map.empty[Char, State]) {
      case (acc, state(in, writeIf0, moveIf0, continueIf0, writeIf1, moveIf1, continueIf1)) =>
        val newState = new State {
          def nextValue(current: Int): Int     = if (current == 0) writeIf0.toInt else writeIf1.toInt
          def positionDelta(current: Int): Int = if (current == 0) delta(moveIf0) else delta(moveIf1)
          def nextState(current: Int): Char    = if (current == 0) continueIf0.head else continueIf1.head
        }
        acc.updated(in.head, newState)
      case (acc, _) => acc
    }

    (startingState.head, steps.toInt, states)
  }

  override def part1(input: (Char, Int, Map[Char, State])): Int = {
    val (start, steps, states) = input

    @tailrec
    def helper(
        position: Int = 0,
        currentState: State = states(start),
        tape: Map[Int, Int] = Map.empty[Int, Int].withDefaultValue(0),
        currentIteration: Int = 0
    ): Int = if (currentIteration >= steps) {
      tape.values.sum
    } else {
      val current = tape(position)
      helper(
        position + currentState.positionDelta(current),
        states(currentState.nextState(current)),
        tape.updated(position, currentState.nextValue(current)),
        currentIteration + 1
      )
    }

    helper()
  }
}
