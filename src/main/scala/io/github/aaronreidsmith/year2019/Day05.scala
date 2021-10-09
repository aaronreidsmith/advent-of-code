package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.util.FileUtils
import io.github.aaronreidsmith.year2019.intcode.IntCode

import scala.io.Source

object Day05 extends FileUtils {
  def main(args: Array[String]): Unit = {
    val instructions = using(Source.fromResource("2019/day05.txt"))(_.mkString.split(',').map(_.toInt).toVector)
    val intCode      = new IntCode(instructions)
    intCode.run() // Use input = 1 for part 1 and input = 5 for part 2
  }
}
