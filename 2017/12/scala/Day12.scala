import scala.collection.mutable
import scala.io.Source
import scala.language.implicitConversions

object Day12 {
  // Utils for converting between immutable and mutable maps
  private implicit def mutableToImmutable(map: mutable.Map[Int, List[Int]]): Map[Int, List[Int]] = Map(map.toSeq: _*)
  private implicit def immutableToMutable(map: Map[Int, List[Int]]): mutable.Map[Int, List[Int]] =
    mutable.Map(map.toSeq: _*)

  private val pipeEntry = "^(\\d+) <-> (.*)".r("id", "neighbors")

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args.head)

    val directRelationships = input.getLines().foldLeft(Map.empty[Int, List[Int]]) {
      case (acc, pipeEntry(pipe, neighbors)) =>
        val id          = pipe.toInt
        val neighborIds = neighbors.split(", ").map(_.toInt).toList
        acc + (id -> neighborIds)
    }
    input.close()

    println(s"Part 1: ${part1(directRelationships).size}")
    println(s"Part 2: ${part2(directRelationships)}")
  }

  private def part1(programs: Map[Int, List[Int]], key: Int = 0): mutable.Set[Int] = {
    val connectedPrograms = mutable.Set(key)
    val lookupQueue       = mutable.Queue(key)
    while (lookupQueue.nonEmpty) {
      val newConnected = programs(lookupQueue.dequeue())
      newConnected.foreach { program =>
        if (!connectedPrograms.contains(program)) {
          connectedPrograms.add(program)
          lookupQueue.enqueue(program)
        }
      }
    }
    connectedPrograms
  }

  private def part2(programs: mutable.Map[Int, List[Int]]): Int = {
    var groups = 0
    while (programs.nonEmpty) {
      val group = part1(programs, programs.keys.head)
      group.foreach(programs -= _)
      groups += 1
    }
    groups
  }

}
