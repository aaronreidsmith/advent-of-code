package io.github.aaronreidsmith.year2019.intcode.util

import scala.util.Try

case class ParsedInstruction(opcode: Int, arg1Mode: ParameterMode, arg2Mode: ParameterMode, arg3Mode: ParameterMode)

object ParsedInstruction {
  def apply(num: Long): ParsedInstruction = {
    val asString = num.toString
    val opCode   = asString.takeRight(2).toInt
    val arg1Mode = ParameterMode(Try(asString.dropRight(2).takeRight(1).toInt).getOrElse(0))
    val arg2Mode = ParameterMode(Try(asString.dropRight(3).takeRight(1).toInt).getOrElse(0))
    val arg3Mode = ParameterMode(Try(asString.dropRight(4).takeRight(1).toInt).getOrElse(1))
    ParsedInstruction(opCode, arg1Mode, arg2Mode, arg3Mode)
  }
}
