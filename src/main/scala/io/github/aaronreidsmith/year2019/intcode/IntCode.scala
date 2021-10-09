package io.github.aaronreidsmith.year2019.intcode

import io.github.aaronreidsmith.year2019.intcode.util.{Immediate, ParameterMode, ParsedInstruction, Position}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

class IntCode(initialInstructions: Seq[Int]) {
  private val instructions = mutable.IndexedSeq(initialInstructions: _*)

  @tailrec
  final def run(pointer: Int = 0): Int = {
    val parsedInstruction = ParsedInstruction(instructions(pointer))
    parsedInstruction.opcode match {
      // Add 2 args
      case 1 =>
        val arg1        = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2        = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex = getValue(pointer + 3, Immediate)
        instructions.update(targetIndex, arg1 + arg2)
        run(pointer + 4)
      // Multiply 2 args
      case 2 =>
        val arg1        = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2        = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex = getValue(pointer + 3, Immediate)
        instructions.update(targetIndex, arg1 * arg2)
        run(pointer + 4)
      // Take input and write to location
      case 3 =>
        print("What is the input to the program (must be an integer)? ")
        val input         = StdIn.readInt()
        val writePosition = getValue(pointer + 1, Immediate)
        instructions.update(writePosition, input)
        run(pointer + 2)
      // Output a value
      case 4 =>
        val value = getValue(pointer + 1, parsedInstruction.arg1Mode)
        println(s"Output: $value")
        run(pointer + 2)
      // Jump if true
      case 5 =>
        val arg1       = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val newPointer = if (arg1 != 0) getValue(pointer + 2, parsedInstruction.arg2Mode) else pointer + 3
        run(newPointer)
      // Jump if false
      case 6 =>
        val arg1       = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val newPointer = if (arg1 == 0) getValue(pointer + 2, parsedInstruction.arg2Mode) else pointer + 3
        run(newPointer)
      // Less than
      case 7 =>
        val arg1         = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2         = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex  = getValue(pointer + 3, Immediate)
        val valueToWrite = if (arg1 < arg2) 1 else 0
        instructions.update(targetIndex, valueToWrite)
        run(pointer + 4)
      // Equal
      case 8 =>
        val arg1         = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2         = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex  = getValue(pointer + 3, Immediate)
        val valueToWrite = if (arg1 == arg2) 1 else 0
        instructions.update(targetIndex, valueToWrite)
        run(pointer + 4)
      // Halt
      case 99    => instructions.head
      case other => throw new IllegalArgumentException(s"$other is not a supported op code")
    }
  }

  private def getValue(pointer: Int, mode: ParameterMode): Int = mode match {
    case Position  => instructions(instructions(pointer))
    case Immediate => instructions(pointer)
  }
}
