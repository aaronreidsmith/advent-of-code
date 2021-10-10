package io.github.aaronreidsmith.year2019.intcode

import io.github.aaronreidsmith.year2019.intcode.util._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

class IntCode(initialInstructions: Map[Long, Long], inputInstruction: Option[Long] = None) {
  private val instructions = mutable.Map(initialInstructions.toSeq: _*).withDefaultValue(0)
  private val output       = mutable.ListBuffer.empty[Long]

  private var initialInput: Option[Long] = inputInstruction
  private var relativeBase: Long         = 0

  def run(): Long = run(0L)

  @tailrec
  private def run(pointer: Long): Long = {
    val parsedInstruction = ParsedInstruction(instructions(pointer))
    parsedInstruction.opcode match {
      // Add 2 args
      case 1 =>
        val arg1        = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2        = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex = getValue(pointer + 3, parsedInstruction.arg3Mode, writing = true)
        instructions.update(targetIndex, arg1 + arg2)
        run(pointer + 4)
      // Multiply 2 args
      case 2 =>
        val arg1        = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2        = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex = getValue(pointer + 3, parsedInstruction.arg3Mode, writing = true)
        instructions.update(targetIndex, arg1 * arg2)
        run(pointer + 4)
      // Take input and write to location
      case 3 =>
        val input = inputInstruction match {
          case Some(instruction) =>
            initialInput = None // Reset this so that it is not reused
            instruction
          case None =>
            print("What is the input to the program (must be an integer)? ")
            StdIn.readLong()
        }
        val writePosition = getValue(pointer + 1, parsedInstruction.arg1Mode, writing = true)
        instructions.update(writePosition, input)
        run(pointer + 2)
      // Output a value
      case 4 =>
        val value = getValue(pointer + 1, parsedInstruction.arg1Mode)
        output.append(value)
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
        val targetIndex  = getValue(pointer + 3, parsedInstruction.arg3Mode, writing = true)
        val valueToWrite = if (arg1 < arg2) 1 else 0
        instructions.update(targetIndex, valueToWrite)
        run(pointer + 4)
      // Equal
      case 8 =>
        val arg1         = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2         = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex  = getValue(pointer + 3, parsedInstruction.arg3Mode, writing = true)
        val valueToWrite = if (arg1 == arg2) 1 else 0
        instructions.update(targetIndex, valueToWrite)
        run(pointer + 4)
      // Update relative base
      case 9 =>
        val arg1 = getValue(pointer + 1, parsedInstruction.arg1Mode)
        relativeBase += arg1
        run(pointer + 2)
      // Halt
      case 99 =>
        if (output.nonEmpty) {
          println(s"Output: ${output.mkString("[", ", ", "]")}")
        }
        instructions(0)
      case other => throw new IllegalArgumentException(s"$other is not a supported op code")
    }
  }

  // https://www.reddit.com/r/adventofcode/comments/e8aw9j/2019_day_9_part_1_how_to_fix_203_error/
  private def getValue(pointer: Long, mode: ParameterMode, writing: Boolean = false): Long = if (pointer >= 0) {
    mode match {
      case Position if writing  => instructions(pointer)
      case Position             => instructions(instructions(pointer))
      case Immediate if writing => instructions(pointer)
      case Immediate            => instructions(pointer)
      case Relative if writing  => instructions(pointer) + relativeBase
      case Relative             => instructions(instructions(pointer) + relativeBase)
    }
  } else {
    throw new IllegalArgumentException("Pointer must be non-negative")
  }
}
