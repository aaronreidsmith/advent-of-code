package io.github.aaronreidsmith.year2019

import scala.io.Source

/* Adapted from https://github.com/maneatingape/advent-of-code/blob/06921c88d77baa8a2b17985fa9c30b35f85096b3/src/main/scala/AdventOfCode2019/Day09.scala
 * since my first try didn't go well. Including their license since it is a substantial portion copied.
 *
 * MIT License
 *
 * Copyright (c) 2021-2022 maneatingape
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
case class IntCode(ip: Long, relativeBase: Long, memory: Map[Long, Long], input: Seq[Long], result: IntCode.State) {
  import IntCode._

  private def read(offset: Int): Long = (memory(ip) / powers(offset)) % 10 match {
    case 0 => memory(memory(ip + offset))
    case 1 => memory(ip + offset)
    case 2 => memory(relativeBase + memory(ip + offset))
    case _ => throw new IllegalArgumentException
  }

  private def write(offset: Int, value: Long): Map[Long, Long] = (memory(ip) / powers(offset)) % 10 match {
    case 0 => memory.updated(memory(ip + offset), value)
    case 2 => memory.updated(relativeBase + memory(ip + offset), value)
    case _ => throw new IllegalArgumentException
  }

  def next: IntCode = memory(ip) % 100 match {
    case 1  => copy(ip = ip + 4, memory = write(3, read(1) + read(2)), result = Running)                // Add
    case 2  => copy(ip = ip + 4, memory = write(3, read(1) * read(2)), result = Running)                // Multiply
    case 3  => copy(ip = ip + 2, memory = write(1, input.head), input = input.tail, result = Running)   // Read
    case 4  => copy(ip = ip + 2, result = Output(read(1)))                                              // Write
    case 5  => copy(ip = if (read(1) != 0) read(2) else ip + 3, result = Running)                       // Jump if true
    case 6  => copy(ip = if (read(1) == 0) read(2) else ip + 3, result = Running)                       // Jump if false
    case 7  => copy(ip = ip + 4, memory = write(3, if (read(1) < read(2)) 1 else 0), result = Running)  // Less than
    case 8  => copy(ip = ip + 4, memory = write(3, if (read(1) == read(2)) 1 else 0), result = Running) // Equals
    case 9  => copy(ip = ip + 2, relativeBase = relativeBase + read(1), result = Running)               // Relative base
    case 99 => copy(result = Halted)
    case _  => throw new IllegalArgumentException
  }

  def withInput(next: Long*): IntCode = copy(input = next)
  def nextOutput: IntCode             = Iterator.iterate(next)(_.next).dropWhile(_.result == Running).next()
  def allOutput: Seq[Long] = {
    val output = Iterator.iterate(this)(_.nextOutput).takeWhile(_.result != Halted)
    output.toSeq.map(_.result).collect { case Output(value) => value }
  }

}

object IntCode {
  private val powers = Map(1 -> 100, 2 -> 1000, 3 -> 10000)

  sealed trait State
  case object Initial            extends State
  case object Running            extends State
  case object Halted             extends State
  case class Output(value: Long) extends State

  def apply(file: Source): IntCode = {
    val memory = file.mkString.trim
      .split(',')
      .zipWithIndex
      .map { case (value, index) => index.toLong -> value.toLong }
      .toMap
    IntCode(0, 0, memory.withDefaultValue(0), Seq(), Initial)
  }
}
