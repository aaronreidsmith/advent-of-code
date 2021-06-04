import scala.annotation.tailrec
import scala.io.Source

object Day7 {
  private case class Node(name: String, weight: Int, children: Seq[Node] = Seq(), childString: Option[String] = None)

  private val noChildren   = "^(.*) \\((\\d+)\\)$".r("name", "weight")
  private val withChildren = "^(.*) \\((\\d+)\\) -> (.*)$".r("name", "weight", "children")

  private var childNodes = Set.empty[String]

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args.head)
    val allNodes = input
      .getLines()
      .map {
        case noChildren(name, weight) => name -> Node(name, weight.toInt)
        case withChildren(name, weight, childString) =>
          name -> Node(name, weight.toInt, childString = Some(childString))
      }
      .toMap
    input.close()

    val mappedNodes = allNodes.map {
      case (name, node) =>
        node.childString match {
          case Some(children) =>
            val mappedChildren = children.split(", ").map(getChildren(_, allNodes))
            name -> Node(name, node.weight, mappedChildren, node.childString)
          case None => name -> node
        }
    }
    val rootName = (mappedNodes.keys.toSet -- childNodes).head
    println(s"Part 1: $rootName")

    val root = mappedNodes(rootName)
    println(s"Part 2: ${traverse(root)}")
  }

  private def getChildren(name: String, mappings: Map[String, Node]): Node = {
    childNodes += name

    val node     = mappings(name)
    val children = node.childString.map(_.split(", ").map(getChildren(_, mappings))).getOrElse(Array())
    Node(node.name, node.weight, children, node.childString)
  }

  @tailrec
  private def traverse(node: Node, parentWeight: Option[Int] = None, parentSiblingWright: Option[Int] = None): Int = {
    val weights       = node.children.map(child => child -> getWeight(child)).toMap
    val outlierWeight = findOutlier(weights.values.toList)
    weights.collectFirst { case (node, weight) if weight == outlierWeight => node } match {
      case Some(outlier) =>
        val nonOutlierWeight = weights.collectFirst { case (n, weight) if weight != outlierWeight => n }.map(getWeight)
        traverse(outlier, Some(getWeight(outlier)), nonOutlierWeight)
      case None =>
        val difference = math.abs(parentWeight.getOrElse(0) - parentSiblingWright.getOrElse(0))
        node.weight - difference
    }
  }

  private def findOutlier(weights: Seq[Int]): Int = weights
    .groupBy(identity)
    .collectFirst {
      case (key, value) if value.size == 1 => key
    }
    .getOrElse(-1)

  private def getWeight(node: Node): Int = node.weight + node.children.foldLeft(0)((acc, n) => acc + getWeight(n))
}
