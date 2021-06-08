import scala.io.Source

object Day16 {
  private val spin     = "^s(\\d+)$".r("amount")
  private val exchange = "^x(\\d+)/(\\d+)$".r("position1", "position2")
  private val partner  = "^p(.*)/(.*)$".r("char1", "char2")

  private sealed trait Move {
    def apply(state: String): String
  }
  private case class Spin(x: Int) extends Move {
    override def apply(state: String): String = state.takeRight(x) ++: state.dropRight(x)
  }
  private case class Exchange(a: Int, b: Int) extends Move {
    override def apply(state: String): String = state.updated(a, state(b)).updated(b, state(a))
  }
  private case class Partner(a: Char, b: Char) extends Move {
    override def apply(state: String): String = Exchange(state.indexOf(a), state.indexOf(b))(state)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args.head)
    val moves = input.mkString
      .split(',')
      .map {
        case spin(amount)                   => Spin(amount.toInt)
        case exchange(position1, position2) => Exchange(position1.toInt, position2.toInt)
        case partner(char1, char2)          => Partner(char1.charAt(0), char2.charAt(0))
        case _                              => throw new IllegalArgumentException
      }
      .toList
    input.close()

    def dance(state: String): String = moves.foldLeft(state)((acc, move) => move(acc))

    val initialState = ('a' to 'p').mkString
    val part1        = dance(initialState)
    println(s"Part 1: $part1")

    val infiniteDancing = Stream.iterate(initialState)(dance)
    val period          = infiniteDancing.indexOf(initialState, 1)
    val part2           = infiniteDancing.drop(1000000000 % period).head
    println(s"Part 2: $part2")
  }
}
