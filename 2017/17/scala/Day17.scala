import scala.annotation.tailrec

object Day17 {
  def main(args: Array[String]): Unit = {
    val input = 324
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  @tailrec
  private def part1(
      stepCount: Int,
      currentPosition: Int = 0,
      currentNum: Int = 0,
      circularList: Vector[Int] = Vector(0)
  ): Int = if (currentNum == 2017) {
    circularList(currentPosition + 1)
  } else {
    val nextPosition = (currentPosition + stepCount) % circularList.size + 1
    val nextNum      = currentNum + 1
    val newList      = (circularList.take(nextPosition) :+ nextNum) ++: circularList.drop(nextPosition)
    part1(stepCount, nextPosition, nextNum, newList)
  }

  private def part2(stepCount: Int): Int = {
    var second: Option[Int] = None
    var curr                = 0
    (1 to 50000000).foreach { i =>
      curr = 1 + (curr + stepCount) % i
      if (curr == 1) second = Some(i)
    }
    second.get
  }
}
