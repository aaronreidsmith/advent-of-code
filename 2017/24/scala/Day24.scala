import scala.io.Source

object Day24 {
  protected[this] case class Component(left: Int, right: Int) {
    val strength: Int = left + right

    def contains(value: Int): Boolean = left == value || right == value
    def other(i: Int): Int            = if (i == left) right else left
  }

  protected[this] type Bridge = List[Component]

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args.head)
    val components = input
      .getLines()
      .foldLeft(Set.empty[Component]) { (acc, line) =>
        val Array(left, right, _*) = line.split('/')
        acc + Component(left.toInt, right.toInt)
      }
    input.close()

    println(s"Part 1: ${part1(components)}")
    println(s"Part 2: ${part2(components)}")
  }

  private def part1(components: Set[Component]): Int = maxStrength(validBridges(components))

  private def part2(components: Set[Component]): Int = {
    val bridges        = validBridges(components).toSet
    val longestBridge  = bridges.map(_.length).max
    val longestBridges = bridges.filter(_.length == longestBridge).iterator
    maxStrength(longestBridges)
  }

  private def maxStrength(bridges: Iterator[Bridge]): Int = bridges.map(_.foldLeft(0)(_ + _.strength)).max

  private def validBridges(components: Set[Component], port: Int = 0): Iterator[Bridge] = {
    val portComponents = components.filter(_.contains(port))
    if (portComponents.nonEmpty) {
      for {
        component <- portComponents.iterator
        bridge    <- validBridges(components - component, component.other(port))
      } yield component :: bridge
    } else Iterator.single(Nil)
  }

}
