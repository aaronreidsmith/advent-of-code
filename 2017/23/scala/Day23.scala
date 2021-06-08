import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day23 {
  private val set = "^set (.*) (.*)$".r("register", "value")
  private val sub = "^sub (.*) (.*)$".r("register", "value")
  private val mul = "^mul (.*) (.*)$".r("register", "value")
  private val jnz = "^jnz (.*) (.*)$".r("value", "offset")

  def main(args: Array[String]): Unit = {
    val input        = Source.fromFile(args.head)
    val instructions = input.getLines().toVector
    input.close()

    println(s"Part 1: ${part1(instructions)}")
    println(s"Part 2: $part2")
  }

  @tailrec
  def part1(
      instructions: Vector[String],
      position: Int = 0,
      registers: Map[String, Long] =
        Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0, "e" -> 0, "f" -> 0, "g" -> 0, "h" -> 0),
      mulsSeen: Int = 0
  ): Int = if (position < 0 || position >= instructions.size) {
    mulsSeen
  } else {
    instructions(position) match {
      case set(register, value) =>
        val newValue         = getValue(registers, value)
        val updatedRegisters = registers + (register -> newValue)
        part1(instructions, position + 1, updatedRegisters, mulsSeen)
      case sub(register, value) =>
        val existingValue    = registers(register)
        val delta            = getValue(registers, value)
        val updatedRegisters = registers + (register -> (existingValue - delta))
        part1(instructions, position + 1, updatedRegisters, mulsSeen)
      case mul(register, value) =>
        val existingValue    = registers(register)
        val factor           = getValue(registers, value)
        val updatedRegisters = registers + (register -> (existingValue * factor))
        part1(instructions, position + 1, updatedRegisters, mulsSeen + 1)
      case jnz(value, offset) =>
        val jump  = getValue(registers, offset).toInt
        val check = getValue(registers, value)
        val delta = if (check != 0) jump else 1
        part1(instructions, position + delta, registers, mulsSeen)
      case other => throw new IllegalArgumentException(other)
    }
  }

  // These numbers were found by just printing the registers of part 1 after changing "a" -> 1
  private def part2: Int = (108400 to 125400 by 17).count(notPrime)

  private def notPrime(n: Int): Boolean = (2 to math.sqrt(n).toInt).exists(n % _ == 0)

  private def getValue(registers: Map[String, Long], maybeValue: String): Long = Try(maybeValue.toInt) match {
    case Success(value) => value
    case Failure(_)     => registers.getOrElse(maybeValue, 0)
  }
}
