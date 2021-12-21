package io.github.aaronreidsmith.year2017

import scala.annotation.tailrec

object Day25 {
  // Faster to just write these by hand than parse
  private sealed trait State {
    def nextValue(current: Int): Int
    def positionDelta(current: Int): Int
    def nextState(current: Int): State
  }
  private case object A extends State {
    override def nextValue(current: Int): Int     = if (current == 0) 1 else 0
    override def positionDelta(current: Int): Int = if (current == 0) 1 else -1
    override def nextState(current: Int): State   = B
  }
  private case object B extends State {
    override def nextValue(current: Int): Int     = if (current == 0) 0 else 1
    override def positionDelta(current: Int): Int = if (current == 0) 1 else -1
    override def nextState(current: Int): State   = if (current == 0) C else B
  }
  private case object C extends State {
    override def nextValue(current: Int): Int     = if (current == 0) 1 else 0
    override def positionDelta(current: Int): Int = if (current == 0) 1 else -1
    override def nextState(current: Int): State   = if (current == 0) D else A
  }
  private case object D extends State {
    override def nextValue(current: Int): Int     = 1
    override def positionDelta(current: Int): Int = -1
    override def nextState(current: Int): State   = if (current == 0) E else F
  }
  private case object E extends State {
    override def nextValue(current: Int): Int     = if (current == 0) 1 else 0
    override def positionDelta(current: Int): Int = -1
    override def nextState(current: Int): State   = if (current == 0) A else D
  }
  private case object F extends State {
    override def nextValue(current: Int): Int     = 1
    override def positionDelta(current: Int): Int = if (current == 0) 1 else -1
    override def nextState(current: Int): State   = if (current == 0) A else E
  }

  def main(args: Array[String]): Unit = {
    val iterations = 12629077 // from input
    println(s"Part 1: ${solution(iterations)}")
  }

  @tailrec
  private def solution(
      stopCondition: Int,
      position: Int = 0,
      currentState: State = A,
      tape: Map[Int, Int] = Map().withDefaultValue(0),
      currentIteration: Int = 0
  ): Int = if (currentIteration >= stopCondition) {
    tape.values.sum
  } else {
    val current = tape(position)
    solution(
      stopCondition,
      position + currentState.positionDelta(current),
      currentState.nextState(current),
      tape.updated(position, currentState.nextValue(current)),
      currentIteration + 1
    )
  }
}
