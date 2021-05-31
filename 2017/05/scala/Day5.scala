import scala.annotation.tailrec
import scala.io.Source

object Day5 {
  def main(args: Array[String]): Unit = {
    val input        = Source.fromFile(args.head)
    val instructions = input.getLines().map(_.toInt).toList
    input.close()

    println(s"Part 1: ${solution(instructions)}")
    println(s"Part 2: ${solution(instructions, part1 = false)}")
  }

  @tailrec
  private def solution(instructions: List[Int], part1: Boolean = true, index: Int = 0, steps: Int = 0): Int = {
    if (index < 0 || index >= instructions.size) {
      steps
    } else {
      val instruction = instructions(index)
      val nextIndex   = instruction + index
      val updatedInstructions =
        instructions.take(index) ++: {
          (if (!part1 && instruction >= 3) instruction - 1 else instruction + 1) :: instructions.drop(index + 1)
        }
      solution(updatedInstructions, part1, nextIndex, steps + 1)
    }
  }
}
