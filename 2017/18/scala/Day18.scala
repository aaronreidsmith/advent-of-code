import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day18 {
  private val snd = "^snd (.*)$".r("x")
  private val set = "^set (.*) (.*)$".r("x", "y")
  private val add = "^add (.*) (.*)$".r("x", "y")
  private val mul = "^mul (.*) (.*)$".r("x", "y")
  private val mod = "^mod (.*) (.*)$".r("x", "y")
  private val rcv = "^rcv (.*)$".r("x")
  private val jgz = "^jgz (.*) (.*)$".r("x", "y")

  def main(args: Array[String]): Unit = {
    val input        = Source.fromFile(args.head)
    val instructions = input.getLines().toVector
    input.close()

    println(s"Part 1: ${part1(instructions)}")
  }

  @tailrec
  private def part1(
      instructions: Vector[String],
      registers: Map[String, Long] = Map(),
      recoveredFrequency: Option[Long] = None,
      position: Long = 0
  ): Long = if (position < 0 || position >= instructions.size) {
    recoveredFrequency.getOrElse(-1)
  } else {
    instructions(position.toInt) match {
      case snd(register) =>
        val frequency = getValue(registers, register)
        part1(
          instructions,
          if (registers.contains(register)) registers else registers + (register -> 0),
          Some(frequency),
          position + 1
        )
      case set(register, value) =>
        val newValue = getValue(registers, value)
        part1(instructions, registers + (register -> newValue), recoveredFrequency, position + 1)
      case add(register, value) =>
        val existingValue = getValue(registers, register)
        val increase      = getValue(registers, value)
        part1(instructions, registers + (register -> (existingValue + increase)), recoveredFrequency, position + 1)
      case mul(register, value) =>
        val existingValue = getValue(registers, register)
        val factor        = getValue(registers, value)
        part1(instructions, registers + (register -> (existingValue * factor)), recoveredFrequency, position + 1)
      case mod(register, value) =>
        val existingValue = getValue(registers, register)
        val divisor       = getValue(registers, value)
        part1(instructions, registers + (register -> (existingValue % divisor)), recoveredFrequency, position + 1)
      case rcv(register) =>
        val registerValue = getValue(registers, register)
        if (registerValue != 0) recoveredFrequency.getOrElse(-1)
        else part1(instructions, registers, recoveredFrequency, position + 1)
      case jgz(register, value) =>
        val registerValue = getValue(registers, register)
        val jumpValue     = getValue(registers, value)
        part1(
          instructions,
          registers,
          recoveredFrequency,
          if (registerValue > 0) position + jumpValue else position + 1
        )
      case _ => throw new IllegalArgumentException
    }
  }

  private def getValue(registers: Map[String, Long], maybeValue: String): Long = Try(maybeValue.toInt) match {
    case Success(value) => value
    case Failure(_)     => registers.getOrElse(maybeValue, 0)
  }
}