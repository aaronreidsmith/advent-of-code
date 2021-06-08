import scala.annotation.tailrec

object Direction extends Enumeration {
  type Direction = Value
  val NORTH, EAST, SOUTH, WEST = Value
}

object Day3 {
  import Direction._

  def main(args: Array[String]): Unit = {
    val input = 347991
    println(s"Part 1: ${solution(input)}")
    println(s"Part 2: ${solution(input, part1 = false, grid = Map((0, 0) -> 1, (1, 0) -> 1))}")
  }

  // Start in second square for ease of algorithm
  @tailrec
  private def solution(
      target: Int,
      part1: Boolean = true,
      x: Int = 1,
      y: Int = 0,
      currentNum: Int = 2,
      currentDirection: Direction = NORTH,
      grid: Map[(Int, Int), Int] = Map((0, 0) -> 1, (1, 0) -> 2)
  ): Int = {
    if (currentNum == target && part1) {
      math.abs(x) + math.abs(y)
    } else if (currentNum > target && !part1) {
      currentNum
    } else {
      val squareToLeft = currentDirection match {
        case NORTH => (x - 1, y)
        case EAST  => (x, y + 1)
        case SOUTH => (x + 1, y)
        case WEST  => (x, y - 1)
      }
      val newCurrentDirection = grid.get(squareToLeft) match {
        // If there is something to our left, keep going in same direction
        case Some(_) => currentDirection
        // Otherwise, rotate counterclockwise
        case None =>
          currentDirection match {
            case NORTH => WEST
            case EAST  => NORTH
            case SOUTH => EAST
            case WEST  => SOUTH
          }
      }
      val (newX, newY) = newCurrentDirection match {
        case NORTH => (x, y + 1)
        case EAST  => (x + 1, y)
        case SOUTH => (x, y - 1)
        case WEST  => (x - 1, y)
      }
      val newCurrentNum = if (part1) currentNum + 1 else adjacentSquares(x, y).flatMap(grid.get).sum
      val newGrid       = grid + ((x, y) -> newCurrentNum)
      solution(target, part1, newX, newY, newCurrentNum, newCurrentDirection, newGrid)
    }
  }

  private def adjacentSquares(x: Int, y: Int): Seq[(Int, Int)] = Seq(
    // Horizontal neighbors
    (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1),
    // Diagonal neighbors
    (x + 1, y + 1),
    (x + 1, y - 1),
    (x - 1, y + 1),
    (x - 1, y - 1)
  )
}
