import scala.annotation.tailrec
import scala.io.Source

object Day6 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args.head)
    val banks = input.mkString.split('\t').map(_.toInt).toList
    input.close()

    val (infiniteState, part1) = partOne(banks)
    println(s"Part 1: $part1")
    println(s"Part 2: ${partTwo(infiniteState, infiniteState)}")
  }

  @tailrec
  private def partOne(currentState: List[Int], seen: Set[List[Int]] = Set(), iterations: Int = 0): (List[Int], Int) = {
    if (seen.contains(currentState)) {
      (currentState, iterations)
    } else {
      val (maxBank, maxIndex) = currentState.zipWithIndex.maxBy(_._1)

      val newState = currentState.toArray
      newState(maxIndex) = 0

      var currentIndex = maxIndex + 1
      (0 until maxBank).foreach { _ =>
        if (currentIndex > 15) currentIndex = 0
        newState(currentIndex) += 1
        currentIndex += 1
      }

      partOne(newState.toList, seen + currentState, iterations + 1)
    }
  }

  @tailrec
  private def partTwo(currentState: List[Int], startingState: List[Int], iterations: Int = 0): Int = {
    if (iterations > 0 && currentState == startingState) {
      iterations
    } else {
      val (maxBank, maxIndex) = currentState.zipWithIndex.maxBy(_._1)

      val newState = currentState.toArray
      newState(maxIndex) = 0

      var currentIndex = maxIndex + 1
      (0 until maxBank).foreach { _ =>
        if (currentIndex > 15) currentIndex = 0
        newState(currentIndex) += 1
        currentIndex += 1
      }

      partTwo(newState.toList, startingState, iterations + 1)
    }
  }
}
