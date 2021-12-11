package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day04 {
  private case class Square(value: Int, hasBeenSeen: Boolean = false)
  private case class Board(grid: Map[(Int, Int), Square]) {
    // Kinda verbose, but it works 🤷
    private val winningPositions = Seq(
      // All rows
      Seq((0, 0), (0, 1), (0, 2), (0, 3), (0, 4)),
      Seq((1, 0), (1, 1), (1, 2), (1, 3), (1, 4)),
      Seq((2, 0), (2, 1), (2, 2), (2, 3), (2, 4)),
      Seq((3, 0), (3, 1), (3, 2), (3, 3), (3, 4)),
      Seq((4, 0), (4, 1), (4, 2), (4, 3), (4, 4)),
      // All columns
      Seq((0, 0), (1, 0), (2, 0), (3, 0), (4, 0)),
      Seq((0, 1), (1, 1), (2, 1), (3, 1), (4, 1)),
      Seq((0, 2), (1, 2), (2, 2), (3, 2), (4, 2)),
      Seq((0, 3), (1, 3), (2, 3), (3, 3), (4, 3)),
      Seq((0, 4), (1, 4), (2, 4), (3, 4), (4, 4))
    )

    def isWinner: Boolean = winningPositions.exists(_.forall(position => grid(position).hasBeenSeen))
    def markSeen(value: Int): Board = {
      val maybePosition = grid.collectFirst { case (position, square) if square.value == value => position }
      maybePosition match {
        case Some(position) => this.copy(grid.updated(position, Square(value, hasBeenSeen = true)))
        case None           => this
      }
    }
    def score(multiplier: Int): Int = multiplier * grid.values.foldLeft(0) {
      case (acc, square) if !square.hasBeenSeen => acc + square.value
      case (acc, _)                             => acc
    }
    // Since we will continue playing the game, we can't store the board state when we win. Instead we store the values
    // of a winning board so we can diff against it later (to find the final winner).
    def values: Set[Int] = grid.values.map(_.value).toSet
  }

  def main(args: Array[String]): Unit = {
    val rawInput = Using.resource(Source.fromResource("2021/day04.txt"))(_.mkString.split("\n\n")).toSeq
    val numbers  = rawInput.head.split(',').map(_.toInt).toList
    val boards = rawInput.tail.foldLeft(List.empty[Board]) { (acc, rawBoard) =>
      val grid = for {
        (line, row)  <- rawBoard.split('\n').zipWithIndex
        (value, col) <- line.trim.split("\\s+").zipWithIndex
      } yield (row, col) -> Square(value.toInt)
      Board(grid.toMap) :: acc
    }
    println(s"Part 1: ${part1(numbers, boards)}")
    println(s"Part 1: ${part2(numbers, boards)}")
  }

  @tailrec
  private def part1(numbers: List[Int], boards: List[Board], previousNumber: Int = 0): Int = {
    val maybeWinner = boards.collectFirst { case board if board.isWinner => board }
    maybeWinner match {
      case Some(winner) => winner.score(previousNumber)
      case None =>
        numbers match {
          case Nil          => throw new RuntimeException("Shouldn't be able to make it this far without finding a winner")
          case head :: tail => part1(tail, boards.map(_.markSeen(head)), head)
        }
    }
  }

  @tailrec
  private def part2(
      numbers: List[Int],
      boards: List[Board],
      previousNumber: Int = 0,
      previousWinningBoards: Set[Set[Int]] = Set()
  ): Int = {
    val winners = boards.collect { case board if board.isWinner => board.values }.toSet
    if (winners.size == boards.size) {
      val maybeLastWinner = boards.collectFirst { case board if !previousWinningBoards.contains(board.values) => board }
      maybeLastWinner match {
        case Some(lastWinner) => lastWinner.score(previousNumber)
        case None             => -1
      }
    } else {
      numbers match {
        case Nil          => throw new RuntimeException("Shouldn't be able to make it this far without finding all winners")
        case head :: tail => part2(tail, boards.map(_.markSeen(head)), head, winners)
      }
    }
  }
}
