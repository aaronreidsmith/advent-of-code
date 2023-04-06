package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day04 extends Solution {
  type I  = (List[Int], List[Board])
  type O1 = Int
  type O2 = Int

  case class Square(value: Int, hasBeenSeen: Boolean = false)
  case class Board(grid: Map[Point, Square]) {
    // Kinda verbose, but it works 🤷
    private val winningPositions = {
      // All winning rows
      val rows = {
        for {
          row <- 0 until 5
          col <- 0 until 5
        } yield Point(row, col)
      }.grouped(5).toSeq
      // All winning cols
      val cols = {
        for {
          col <- 0 until 5
          row <- 0 until 5
        } yield Point(row, col)
      }.grouped(5).toSeq
      rows ++ cols
    }

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

  override def parseInput(file: Source): (List[Int], List[Board]) = {
    val raw     = file.mkString.trim.split("\n\n").toSeq
    val numbers = raw.head.split(',').map(_.toInt).toList
    val boards = raw.tail.foldLeft(List.empty[Board]) { (acc, rawBoard) =>
      val grid = for {
        (line, row)  <- rawBoard.split('\n').zipWithIndex
        (value, col) <- line.trim.split("\\s+").zipWithIndex
      } yield Point(row, col) -> Square(value.toInt)
      Board(grid.toMap) :: acc
    }
    (numbers, boards)
  }

  override def part1(input: (List[Int], List[Board])): Int = {
    @tailrec
    def helper(numbers: List[Int], boards: List[Board], previousNumber: Int = 0): Int = boards.find(_.isWinner) match {
      case Some(winner) => winner.score(previousNumber)
      case None =>
        numbers match {
          case Nil => throw new RuntimeException("Shouldn't be able to make it this far without finding a winner")
          case head :: tail => helper(tail, boards.map(_.markSeen(head)), head)
        }
    }

    val (numbers, boards) = input
    helper(numbers, boards)
  }

  override def part2(input: (List[Int], List[Board])): Int = {
    @tailrec
    def helper(
        numbers: List[Int],
        boards: List[Board],
        previousNumber: Int = 0,
        previousWinningBoards: Set[Set[Int]] = Set.empty
    ): Int = {
      val winners = boards.collect { case board if board.isWinner => board.values }.toSet
      if (winners.size == boards.size) {
        val maybeLastWinner = boards.collectFirst {
          case board if !previousWinningBoards.contains(board.values) => board
        }
        maybeLastWinner match {
          case Some(lastWinner) => lastWinner.score(previousNumber)
          case None             => -1
        }
      } else {
        numbers match {
          case Nil => throw new RuntimeException("Shouldn't be able to make it this far without finding all winners")
          case head :: tail => helper(tail, boards.map(_.markSeen(head)), head, winners)
        }
      }
    }

    val (numbers, boards) = input
    helper(numbers, boards)
  }
}
