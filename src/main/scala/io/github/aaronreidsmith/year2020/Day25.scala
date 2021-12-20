package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

object Day25 {
  def main(args: Array[String]): Unit = {
    val List(cardKey, doorKey, _*) = using("2020/day25.txt")(_.getLines().toList.map(_.toInt))

    @tailrec
    def solution(handshake: Long, target: Long): Long = if (target == doorKey) {
      handshake
    } else {
      val newTarget    = (target * 7)          % 20201227
      val newHandshake = (handshake * cardKey) % 20201227
      solution(newHandshake, newTarget)
    }

    println(s"Part 1: ${solution(1L, 1L)}")
  }
}
