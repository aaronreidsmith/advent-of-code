package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution
import org.apache.commons.text.StringEscapeUtils

import scala.io.Source

object Day08 extends Solution(2015, 8) {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

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

  override protected[year2015] def parseInput(file: Source): List[String] = file.getLines().toList

  override protected[year2015] def part1(input: List[String]): Int = {
    val (chars, memory) = input.foldLeft((0, 0)) {
      case ((charAcc, memAcc), line) => (charAcc + line.length, memAcc + line.memoryLength)
      case (acc, _)                  => acc
    }
    chars - memory
  }

  override protected[year2015] def part2(input: List[String]): Int = {
    val (reEncoded, chars) = input.foldLeft((0, 0)) {
      case ((reEncodedAcc, charsAcc), line) => (reEncodedAcc + line.reEncodedLength, charsAcc + line.length)
      case (acc, _)                         => acc
    }
    reEncoded - chars
  }
}
