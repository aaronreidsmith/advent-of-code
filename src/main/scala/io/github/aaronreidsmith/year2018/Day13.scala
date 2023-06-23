package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.*

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends Solution {
  type I  = (Vector[String], Vector[Cart])
  type O1 = String
  type O2 = String

  sealed trait Turn
  case object Left     extends Turn
  case object Right    extends Turn
  case object Straight extends Turn

  case class Cart(
      position: Point,
      direction: Direction,
      currentTurn: Turn = Left,
      dead: Boolean = false
  ) {
    def move: Cart = this.copy(position = position.move(direction))
    def turn: Cart = currentTurn match {
      case Left     => this.copy(direction = direction.left, currentTurn = Straight)
      case Straight => this.copy(currentTurn = Right)
      case Right    => this.copy(direction = direction.right, currentTurn = Left)
    }
  }

  override def parseInput(file: Source): (Vector[String], Vector[Cart]) = {
    file.getLines().zipWithIndex.foldLeft((Vector.empty[String], Vector.empty[Cart])) {
      case ((track, carts), (line, row)) =>
        val newCarts = line.zipWithIndex.collect {
          case (char, col) if "^v<>".contains(char) => Cart(Point(row, col), Direction.fromChar(char))
        }
        val updatedLine = line.replaceAll("[\\^v]", "|").replaceAll("[<>]", "-")
        (track :+ updatedLine, carts ++ newCarts)
    }
  }

  override def part1(input: (Vector[String], Vector[Cart])): String = {
    val (track, initialCarts) = input

    @tailrec
    def helper(carts: Vector[Cart], pointer: Int = 0): String = {
      carts.groupBy(_.position).view.mapValues(_.length).collectFirst {
        case (pos, count) if count > 1 => s"${pos.y},${pos.x}"
      } match {
        case Some(answer) => answer
        case None =>
          val currentCart = carts(pointer)
          val moved       = currentCart.move
          val turned = track(moved.position.x)(moved.position.y) match {
            case '+'                              => moved.turn
            case '/' if moved.direction == North  => moved.copy(direction = East)
            case '/' if moved.direction == East   => moved.copy(direction = North)
            case '/' if moved.direction == South  => moved.copy(direction = West)
            case '/' if moved.direction == West   => moved.copy(direction = South)
            case '\\' if moved.direction == North => moved.copy(direction = West)
            case '\\' if moved.direction == East  => moved.copy(direction = South)
            case '\\' if moved.direction == South => moved.copy(direction = East)
            case '\\' if moved.direction == West  => moved.copy(direction = North)
            case _                                => moved
          }
          val nextPointer  = (pointer + 1) % carts.length
          val updatedCarts = carts.updated(pointer, turned)
          val sortedCarts  = if (nextPointer == 0) updatedCarts.sortBy(_.position) else updatedCarts
          helper(sortedCarts, nextPointer)
      }
    }

    helper(initialCarts)
  }

  override def part2(input: (Vector[String], Vector[Cart])): String = {
    val (track, initialCarts) = input

    @tailrec
    def helper(carts: Vector[Cart], pointer: Int = 0): String = carts match {
      case Vector(head) => s"${head.position.y},${head.position.x}"
      case _ =>
        val currentCart = carts(pointer)
        val turned = if (currentCart.dead) {
          currentCart
        } else {
          val moved = currentCart.move
          track(moved.position.x)(moved.position.y) match {
            case '+'                              => moved.turn
            case '/' if moved.direction == North  => moved.copy(direction = East)
            case '/' if moved.direction == East   => moved.copy(direction = North)
            case '/' if moved.direction == South  => moved.copy(direction = West)
            case '/' if moved.direction == West   => moved.copy(direction = South)
            case '\\' if moved.direction == North => moved.copy(direction = West)
            case '\\' if moved.direction == East  => moved.copy(direction = South)
            case '\\' if moved.direction == South => moved.copy(direction = East)
            case '\\' if moved.direction == West  => moved.copy(direction = North)
            case _                                => moved
          }
        }
        val updatedCarts = carts.updated(pointer, turned)
        val withCartsMarkedDead = updatedCarts.groupBy(_.position).view.mapValues(_.length).collectFirst {
          case (position, count) if count > 1 => position
        } match {
          case Some(position) =>
            updatedCarts.collect {
              case cart if cart.position == position => cart.copy(dead = true)
              case cart                              => cart
            }
          case None => updatedCarts
        }
        val nextPointer = (pointer + 1) % withCartsMarkedDead.length
        val sortedCarts = if (nextPointer == 0) {
          withCartsMarkedDead.filterNot(_.dead).sortBy(_.position)
        } else {
          withCartsMarkedDead
        }
        helper(sortedCarts, nextPointer)
    }

    helper(initialCarts)
  }
}
