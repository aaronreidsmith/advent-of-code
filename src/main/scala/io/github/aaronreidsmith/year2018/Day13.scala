package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.*

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends Solution {
  type I  = (Vector[String], Vector[Cart])
  type O1 = String
  type O2 = String

  enum Turn {
    case Left, Right, Straight
  }

  case class Cart(
      position: Point,
      direction: Direction,
      currentTurn: Turn = Turn.Left,
      dead: Boolean = false
  ) {
    def move: Cart = this.copy(position = position.move(direction))
    def turn: Cart = currentTurn match {
      case Turn.Left     => this.copy(direction = direction.left, currentTurn = Turn.Straight)
      case Turn.Straight => this.copy(currentTurn = Turn.Right)
      case Turn.Right    => this.copy(direction = direction.right, currentTurn = Turn.Left)
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

          val turned = (track(moved.position.x)(moved.position.y), moved.direction) match {
            case ('+', _)                => moved.turn
            case ('/', Direction.North)  => moved.copy(direction = Direction.East)
            case ('/', Direction.East)   => moved.copy(direction = Direction.North)
            case ('/', Direction.South)  => moved.copy(direction = Direction.West)
            case ('/', Direction.West)   => moved.copy(direction = Direction.South)
            case ('\\', Direction.North) => moved.copy(direction = Direction.West)
            case ('\\', Direction.East)  => moved.copy(direction = Direction.South)
            case ('\\', Direction.South) => moved.copy(direction = Direction.East)
            case ('\\', Direction.West)  => moved.copy(direction = Direction.North)
            case _                       => moved
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
          (track(moved.position.x)(moved.position.y), moved.direction) match {
            case ('+', _)                => moved.turn
            case ('/', Direction.North)  => moved.copy(direction = Direction.East)
            case ('/', Direction.East)   => moved.copy(direction = Direction.North)
            case ('/', Direction.South)  => moved.copy(direction = Direction.West)
            case ('/', Direction.West)   => moved.copy(direction = Direction.South)
            case ('\\', Direction.North) => moved.copy(direction = Direction.West)
            case ('\\', Direction.East)  => moved.copy(direction = Direction.South)
            case ('\\', Direction.South) => moved.copy(direction = Direction.East)
            case ('\\', Direction.West)  => moved.copy(direction = Direction.North)
            case _                       => moved
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
