import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val Array(input, _*) = args
    val checksum = Source.fromFile(input).getLines().foldLeft(0) {
      case (acc, line) =>
        val nums = line.split('\t').map(_.toInt)
        // Part 1
        if (args.length == 1) {
          acc + (nums.max - nums.min)
        } else {
          val evenPair = nums
            .combinations(2)
            .collectFirst {
              case Array(a, b) if a % b == 0 || b % a == 0 => if (a >= b) a / b else b / a
            }
            .get // Hate this, but it's not production code
          acc + evenPair
        }
    }
    println(checksum)
  }
}
