package io.github.aaronreidsmith.year2018

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Ordered.orderingToOrdered

object Day13 {
  protected[this] object Direction extends Enumeration {
    type Direction = Value
    val North, East, South, West = Value
  }
  import Direction._

  protected[this] object Turn extends Enumeration {
    type Turn = Value
    val Left, Straight, Right = Value
  }
  import Turn._

  protected[this] case class Cart(
      row: Int,
      col: Int,
      direction: Direction,
      currentTurn: Turn = Left,
      dead: Boolean = false
  ) extends Ordered[Cart] {
    val position: (Int, Int) = (row, col)

    override def compare(that: Cart): Int = this.position compare that.position

    def move: Cart = direction match {
      case North => this.copy(row = row - 1)
      case East  => this.copy(col = col + 1)
      case South => this.copy(row = row + 1)
      case West  => this.copy(col = col - 1)
    }

    def turn: Cart = currentTurn match {
      case Left =>
        direction match {
          case North => this.copy(direction = West, currentTurn = Straight)
          case East  => this.copy(direction = North, currentTurn = Straight)
          case South => this.copy(direction = East, currentTurn = Straight)
          case West  => this.copy(direction = South, currentTurn = Straight)
        }
      case Straight => this.copy(currentTurn = Right)
      case Right =>
        direction match {
          case North => this.copy(direction = East, currentTurn = Left)
          case East  => this.copy(direction = South, currentTurn = Left)
          case South => this.copy(direction = West, currentTurn = Left)
          case West  => this.copy(direction = North, currentTurn = Left)
        }
    }
  }

  protected[this] implicit class CharOps(char: Char) {
    private val directionMappings = Map(
      '^' -> North,
      '>' -> East,
      'v' -> South,
      '<' -> West
    )

    def isCart: Boolean        = directionMappings.contains(char)
    def toDirection: Direction = directionMappings.getOrElse(char, throw new IllegalArgumentException)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2018/day13.txt")
    val (track, carts) = input.getLines().zipWithIndex.foldLeft((Vector.empty[String], Vector.empty[Cart])) {
      case ((trackAcc, cartAcc), (line, row)) =>
        val newCarts = line.zipWithIndex.collect {
          case (char, col) if char.isCart => Cart(row, col, char.toDirection)
        }
        val updatedLine = line.replaceAll("[\\^v]", "|").replaceAll("[<>]", "-")
        (trackAcc :+ updatedLine, cartAcc ++ newCarts)
    }
    input.close()

    @tailrec
    def part1(carts: Vector[Cart], pointer: Int = 0): String =
      carts.groupBy(_.position).view.mapValues(_.length).collectFirst {
        case ((row, col), count) if count > 1 => s"$col,$row"
      } match {
        case Some(answer) => answer
        case None =>
          val currentCart = carts(pointer)
          val moved       = currentCart.move
          val turned = track(moved.row)(moved.col) match {
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
          val sortedCarts  = if (nextPointer == 0) updatedCarts.sorted else updatedCarts
          part1(sortedCarts, nextPointer)
      }

    @tailrec
    def part2(carts: Vector[Cart], pointer: Int = 0): String = carts match {
      case Vector(head) =>
        val (row, col) = head.position
        s"$col,$row"
      case _ =>
        val currentCart = carts(pointer)
        val turned = if (currentCart.dead) {
          currentCart
        } else {
          val moved = currentCart.move
          track(moved.row)(moved.col) match {
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
        val sortedCarts = if (nextPointer == 0) withCartsMarkedDead.filterNot(_.dead).sorted else withCartsMarkedDead
        part2(sortedCarts, nextPointer)
    }

    println(s"Part 1: ${part1(carts)}")
    println(s"Part 2: ${part2(carts)}")
  }
}
