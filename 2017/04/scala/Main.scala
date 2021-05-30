import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input       = args.head
    val passphrases = Source.fromFile(input).getLines()
    val part1 = passphrases.filter { line =>
      val words = line.split(' ')
      words.distinct.length == words.length
    }.toList // So we can use it again below
    println(s"Part 1: ${part1.size}")

    val part2 = part1.count { line =>
      val words = line.split(' ').map(_.sorted)
      words.distinct.length == words.length
    }
    println(s"Part 2: $part2")
  }
}
