package io.github.aaronreidsmith.year2019.intcode

import io.github.aaronreidsmith.year2019.intcode.util._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

class IntCode(initialInstructions: Map[Long, Long], initialInput: Seq[Long] = Seq(), suspendOnOutput: Boolean = false) {
  private val instructions = mutable.Map(initialInstructions.toSeq: _*).withDefaultValue(0)
  private val inputQueue   = mutable.Queue(initialInput: _*)
  private val output       = mutable.ListBuffer.empty[Long]

  private var pointer      = 0L
  private var relativeBase = 0L
  private var finished     = false

  def getRegisterValue(address: Long): Long = instructions(address)
  def getOutput: Vector[Long]               = output.toVector
  def getOutputAsString: String             = output.mkString("[", ", ", "]")
  def isFinished: Boolean                   = finished

  @tailrec
  final def run(additionalInput: Seq[Long] = Seq()): IntCode = {
    inputQueue.enqueue(additionalInput: _*) // Add any additional input to the end of the queue
    val parsedInstruction = ParsedInstruction(instructions(pointer))
    parsedInstruction.opcode match {
      // Add 2 args
      case 1 =>
        val arg1        = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2        = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex = getValue(pointer + 3, parsedInstruction.arg3Mode, writing = true)
        instructions.update(targetIndex, arg1 + arg2)
        pointer += 4
        run()
      // Multiply 2 args
      case 2 =>
        val arg1        = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2        = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex = getValue(pointer + 3, parsedInstruction.arg3Mode, writing = true)
        instructions.update(targetIndex, arg1 * arg2)
        pointer += 4
        run()
      // Take input and write to location
      case 3 =>
        val input = if (inputQueue.nonEmpty) {
          inputQueue.dequeue()
        } else {
          print("What is the input to the program (must be an integer)? ")
          StdIn.readLong()
        }
        val writePosition = getValue(pointer + 1, parsedInstruction.arg1Mode, writing = true)
        instructions.update(writePosition, input)
        pointer += 2
        run()
      // Output a value
      case 4 =>
        val value = getValue(pointer + 1, parsedInstruction.arg1Mode)
        output.append(value)
        pointer += 2
        if (suspendOnOutput) this else run()
      // Jump if true
      case 5 =>
        val arg1 = getValue(pointer + 1, parsedInstruction.arg1Mode)
        pointer = if (arg1 != 0) getValue(pointer + 2, parsedInstruction.arg2Mode) else pointer + 3
        run()
      // Jump if false
      case 6 =>
        val arg1 = getValue(pointer + 1, parsedInstruction.arg1Mode)
        pointer = if (arg1 == 0) getValue(pointer + 2, parsedInstruction.arg2Mode) else pointer + 3
        run()
      // Less than
      case 7 =>
        val arg1         = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2         = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex  = getValue(pointer + 3, parsedInstruction.arg3Mode, writing = true)
        val valueToWrite = if (arg1 < arg2) 1 else 0
        instructions.update(targetIndex, valueToWrite)
        pointer += 4
        run()
      // Equal
      case 8 =>
        val arg1         = getValue(pointer + 1, parsedInstruction.arg1Mode)
        val arg2         = getValue(pointer + 2, parsedInstruction.arg2Mode)
        val targetIndex  = getValue(pointer + 3, parsedInstruction.arg3Mode, writing = true)
        val valueToWrite = if (arg1 == arg2) 1 else 0
        instructions.update(targetIndex, valueToWrite)
        pointer += 4
        run()
      // Update relative base
      case 9 =>
        val arg1 = getValue(pointer + 1, parsedInstruction.arg1Mode)
        relativeBase += arg1
        pointer += 2
        run()
      // Halt
      case 99 =>
        finished = true
        this
      case other => throw new IllegalArgumentException(s"$other is not a supported op code")
    }
  }

  // https://www.reddit.com/r/adventofcode/comments/e8aw9j/2019_day_9_part_1_how_to_fix_203_error/
  private def getValue(pointer: Long, mode: ParameterMode, writing: Boolean = false): Long = if (pointer >= 0) {
    mode match {
      case Position if writing => instructions(pointer)
      case Position            => instructions(instructions(pointer))
      case Immediate           => instructions(pointer)
      case Relative if writing => instructions(pointer) + relativeBase
      case Relative            => instructions(instructions(pointer) + relativeBase)
    }
  } else {
    throw new IllegalArgumentException("Pointer must be non-negative")
  }
}
