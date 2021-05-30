import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val Array(input, _*) = args
    val captcha          = Source.fromFile(input).getLines().mkString.map(_.toString.toInt)

    // Part 1
    val summedCaptcha = if (args.length == 1) {
      (captcha :+ captcha.head).sliding(2).foldLeft(0) {
        case (acc, Vector(a, b)) => acc + (if (a == b) a else 0)
      }
    } else {
      val stepSize = captcha.length / 2
      val rotated  = captcha.drop(stepSize) ++ captcha.take(stepSize)
      captcha.zip(rotated).foldLeft(0) {
        case (acc, (a, b)) => acc + (if (a == b) a else 0)
      }
    }
    println(summedCaptcha)
  }
}
