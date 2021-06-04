import scala.io.Source

object Day2 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args.head)
    val grid  = input.getLines().toList
    input.close()

    val (part1, part2) = grid.foldLeft((0, 0)) {
      case ((part1Acc, part2Acc), line) =>
        val nums = line.split('\t').map(_.toInt)
        val evenPair = nums
          .combinations(2)
          .collectFirst {
            case Array(a, b) if a % b == 0 => a / b
            case Array(a, b) if b % a == 0 => b / a
          }
          .get // Hate this, but it's not production code
        (part1Acc + (nums.max - nums.min), part2Acc + evenPair)
    }
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
