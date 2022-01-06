package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.using
import org.apache.commons.text.StringEscapeUtils

object Day08 {
  private implicit class StringOps(string: String) {
    def memoryLength: Int = {
      val it  = string.drop(1).dropRight(1).iterator
      val out = new StringBuilder
      while (it.hasNext) {
        val char = it.next()
        if (char == '\\') {
          val nextChar = it.next()
          if (nextChar == '"' || nextChar == '\\') {
            out += nextChar
          } else if (nextChar == 'x') {
            val hexValue = Integer.parseInt(it.next().toString + it.next().toString, 16)
            out += hexValue.toChar
          }
        } else {
          out += char
        }
      }
      out.length()
    }

    def reEncodedLength: Int = StringEscapeUtils.escapeJson(string).length + 2 // For the leading and trailing quotes
  }

  def main(args: Array[String]): Unit = {
    val input = using("2015/day08.txt")(_.getLines().toList)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private[year2015] def part1(input: List[String]): Int = {
    val (chars, memory) = input.foldLeft((0, 0)) {
      case ((charAcc, memAcc), line) => (charAcc + line.length, memAcc + line.memoryLength)
    }
    chars - memory
  }

  private[year2015] def part2(input: List[String]): Int = {
    val (reEncoded, chars) = input.foldLeft((0, 0)) {
      case ((reEncodedAcc, charsAcc), line) => (reEncodedAcc + line.reEncodedLength, charsAcc + line.length)
    }
    reEncoded - chars
  }
}
