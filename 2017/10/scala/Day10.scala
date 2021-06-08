import scala.annotation.tailrec
import scala.io.Source

object Day10 {
  def main(args: Array[String]): Unit = {
    val input       = Source.fromFile(args.head)
    val inputString = input.mkString
    input.close()

    val part1Lengths = inputString.split(',').map(_.toInt).toList
    val part2Lengths = List.fill(64)(inputString.map(_.toInt).toList ++: List(17, 31, 73, 47, 23)).flatten

    println(s"Part 1: ${part1(part1Lengths)}")
    println(s"Part 2: ${part2(part2Lengths)}")
  }

  @tailrec
  private def part1(
      reversalLengths: List[Int],
      list: List[Int] = Range(0, 256).toList,
      currentPosition: Int = 0,
      skipSize: Int = 0
  ): Int = reversalLengths match {
    case Nil => list.take(2).product
    case reversalLength :: tail =>
      val leftRotated   = rotateLeft(list, currentPosition)
      val reversedPart  = leftRotated.take(reversalLength).reverse
      val remainingPart = leftRotated.drop(reversalLength)
      val newList       = rotateRight(reversedPart ++: remainingPart, currentPosition)
      part1(tail, newList, (currentPosition + reversalLength + skipSize) % 256, skipSize + 1)
  }

  @tailrec
  private def part2(
      reversalLengths: List[Int],
      list: List[Int] = Range(0, 256).toList,
      currentPosition: Int = 0,
      skipSize: Int = 0
  ): String = reversalLengths match {
    case Nil => list.grouped(16).map(_.reduceLeft(_ ^ _).toHexString).mkString
    case reversalLength :: tail =>
      val leftRotated   = rotateLeft(list, currentPosition)
      val reversedPart  = leftRotated.take(reversalLength).reverse
      val remainingPart = leftRotated.drop(reversalLength)
      val newList       = rotateRight(reversedPart ++: remainingPart, currentPosition)
      part2(tail, newList, (currentPosition + reversalLength + skipSize) % 256, skipSize + 1)
  }

  private def rotateLeft(list: List[Int], i: Int): List[Int] = {
    val size = list.size
    list.drop(i % size) ++: list.take(i % size)
  }

  private def rotateRight(list: List[Int], i: Int): List[Int] = {
    val size = list.size
    list.drop(size - (i % size)) ++: list.take(size - (i % size))
  }
}
