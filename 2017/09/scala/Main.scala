import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val stream                = Source.fromFile(args.head).mkString
    val (score, garbageCount) = solution(stream)
    println(s"Part 1: $score")
    println(s"Part 2: $garbageCount")
  }

  @tailrec
  private def solution(
      stream: String,
      level: Int = 0,
      scores: Map[Int, Int] = Map(1 -> 0),
      garbageCount: Int = 0,
      inGarbage: Boolean = false,
      ignoreChar: Boolean = false
  ): (Int, Int) = stream.headOption match {
    case Some(char) =>
      val newStream = stream.drop(1)
      if (ignoreChar) {
        solution(newStream, level, scores, garbageCount, inGarbage)
      } else {
        char match {
          case '{' if !inGarbage => solution(newStream, level + 1, scores, garbageCount)
          case '}' if !inGarbage =>
            solution(newStream, level - 1, scores + (level -> (scores.getOrElse(level, 0) + 1)), garbageCount)
          case '<' if !inGarbage => solution(newStream, level, scores, garbageCount, inGarbage = true)
          case '>' if inGarbage  => solution(newStream, level, scores, garbageCount)
          case '!' if inGarbage  => solution(newStream, level, scores, garbageCount, inGarbage, ignoreChar = true)
          case _ =>
            val newGarbageCount = if (inGarbage) garbageCount + 1 else garbageCount
            solution(newStream, level, scores, newGarbageCount, inGarbage, ignoreChar)
        }
      }
    case None =>
      val totalScore = scores.foldLeft(0) { case (acc, (level, count)) => acc + (level * count) }
      (totalScore, garbageCount)
  }
}
