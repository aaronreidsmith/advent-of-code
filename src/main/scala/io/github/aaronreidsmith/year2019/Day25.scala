package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{Solution, using}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day25 extends Solution(2019, 25) {
  type I  = IntCode
  type O1 = Int
  type O2 = Nothing

  private implicit class IntCodeOps(intCode: IntCode) {
    def describeSurroundings(printOutput: Boolean): Either[Int, IntCode] = {
      @tailrec
      def helper(computer: IntCode, message: String): Either[Int, IntCode] = {
        if (message.endsWith("airlock.\"")) {
          Left(message.filter(_.isDigit).toInt)
        } else if (message.endsWith("Command?\n") || message.endsWith("!\n\n") || message.endsWith("\"\n")) {
          if (printOutput) {
            print(message)
          }
          Right(computer)
        } else {
          val nextComputer = computer.next
          val nextMessage = nextComputer.result match {
            case IntCode.Output(value) => message + value.toChar
            case _                     => message
          }
          helper(nextComputer, nextMessage)
        }
      }

      helper(intCode, "")
    }
  }

  // Override this so if the user exits it doesn't output a wrong answer
  override def run(): Unit = {
    println("Year 2019, Day 25")
    val input  = using("2019/day25.txt")(parseInput)
    val answer = part1(input)
    if (answer != -1) {
      println(s"Part 1: $answer")
    }
  }

  override protected[year2019] def parseInput(file: Source): IntCode = IntCode(file)

  override protected[year2019] def part1(input: IntCode): Int = {
    // Not a general solution; only works with my input. To play the game yourself, set `autoSolve` to `false` below
    val commands = mutable.Queue(
      "north",
      "east",
      "south",
      "take hypercube",
      "north",
      "west",
      "north",
      "east",
      "take tambourine",
      "west",
      "west",
      "take spool of cat6",
      "north",
      "take weather machine",
      "west",
      "west",
      "west",
      "exit"
    )

    @tailrec
    def helper(computer: IntCode, command: String, autoSolve: Boolean = true): Int = {
      if (command != "exit") {
        val input = (command :+ '\n').map(_.toLong)
        computer.withInput(input: _*).describeSurroundings(!autoSolve) match {
          case Left(password) => password
          case Right(nextComputer) =>
            val nextCommand = if (autoSolve) Try(commands.dequeue()).getOrElse("") else Console.in.readLine()
            helper(nextComputer, nextCommand, autoSolve)
        }
      } else {
        -1
      }
    }

    helper(input, "")
  }
}
