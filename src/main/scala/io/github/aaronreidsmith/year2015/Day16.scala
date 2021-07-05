package io.github.aaronreidsmith.year2015

import scala.io.Source

object Day16 {
  private val sue         = "^Sue (\\d+): (.*)$".r
  private val children    = "^children: (\\d+)$".r
  private val cats        = "^cats: (\\d+)$".r
  private val samoyeds    = "^samoyeds: (\\d+)$".r
  private val pomeranians = "^pomeranians: (\\d+)$".r
  private val akitas      = "^akitas: (\\d+)$".r
  private val vizslas     = "^vizslas: (\\d+)$".r
  private val goldfish    = "^goldfish: (\\d+)$".r
  private val trees       = "^trees: (\\d+)$".r
  private val cars        = "^cars: (\\d+)$".r
  private val perfumes    = "^perfumes: (\\d+)$".r

  protected[this] case class Sue(
      number: Int,
      children: Option[Int] = None,
      cats: Option[Int] = None,
      samoyeds: Option[Int] = None,
      pomeranians: Option[Int] = None,
      akitas: Option[Int] = None,
      vizslas: Option[Int] = None,
      goldfish: Option[Int] = None,
      trees: Option[Int] = None,
      cars: Option[Int] = None,
      perfumes: Option[Int] = None
  )

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2015/day16.txt")
    val sues = input.getLines().foldLeft(List.empty[Sue]) {
      case (acc, sue(num, characteristics)) =>
        val newSue = characteristics.split(", ").foldLeft(Sue(num.toInt)) { (currSue, characteristic) =>
          characteristic match {
            case children(childNum)         => currSue.copy(children = Some(childNum.toInt))
            case cats(catNum)               => currSue.copy(cats = Some(catNum.toInt))
            case samoyeds(samoyedNum)       => currSue.copy(samoyeds = Some(samoyedNum.toInt))
            case pomeranians(pomeranianNum) => currSue.copy(pomeranians = Some(pomeranianNum.toInt))
            case akitas(akitaNum)           => currSue.copy(akitas = Some(akitaNum.toInt))
            case vizslas(vizslaNum)         => currSue.copy(vizslas = Some(vizslaNum.toInt))
            case goldfish(goldfishNum)      => currSue.copy(goldfish = Some(goldfishNum.toInt))
            case trees(treeNum)             => currSue.copy(trees = Some(treeNum.toInt))
            case cars(carNum)               => currSue.copy(cars = Some(carNum.toInt))
            case perfumes(perfumeNum)       => currSue.copy(perfumes = Some(perfumeNum.toInt))
            case _                          => throw new IllegalArgumentException
          }
        }
        acc :+ newSue
      case _ => throw new IllegalArgumentException
    }
    input.close()

    val part1 = sues
      .filter { sue =>
        val childCheck = sue.children match {
          case Some(childNum) => childNum == 3
          case None           => true
        }
        val catCheck = sue.cats match {
          case Some(catNum) => catNum == 7
          case None         => true
        }
        val samoyedCheck = sue.samoyeds match {
          case Some(samoyedNum) => samoyedNum == 2
          case None             => true
        }
        val pomeranianCheck = sue.pomeranians match {
          case Some(pomeranianNum) => pomeranianNum == 3
          case None                => true
        }
        val akitaCheck = sue.akitas match {
          case Some(akitaNum) => akitaNum == 0
          case None           => true
        }
        val vizslaCheck = sue.vizslas match {
          case Some(vizslaNum) => vizslaNum == 0
          case None            => true
        }
        val goldfishCheck = sue.goldfish match {
          case Some(goldfishNum) => goldfishNum == 5
          case None              => true
        }
        val treeCheck = sue.trees match {
          case Some(treeNum) => treeNum == 3
          case None          => true
        }
        val carCheck = sue.cars match {
          case Some(carNum) => carNum == 2
          case None         => true
        }
        val perfumeCheck = sue.perfumes match {
          case Some(perfumeNum) => perfumeNum == 1
          case None             => true
        }

        childCheck && catCheck && samoyedCheck && pomeranianCheck && akitaCheck &&
        vizslaCheck && goldfishCheck && treeCheck && carCheck && perfumeCheck
      }
      .head
      .number
    println(s"Part 1: $part1")

    val part2 = sues
      .filter { sue =>
        val childCheck = sue.children match {
          case Some(childNum) => childNum == 3
          case None           => true
        }
        val catCheck = sue.cats match {
          case Some(catNum) => catNum > 7
          case None         => true
        }
        val samoyedCheck = sue.samoyeds match {
          case Some(samoyedNum) => samoyedNum == 2
          case None             => true
        }
        val pomeranianCheck = sue.pomeranians match {
          case Some(pomeranianNum) => pomeranianNum < 3
          case None                => true
        }
        val akitaCheck = sue.akitas match {
          case Some(akitaNum) => akitaNum == 0
          case None           => true
        }
        val vizslaCheck = sue.vizslas match {
          case Some(vizslaNum) => vizslaNum == 0
          case None            => true
        }
        val goldfishCheck = sue.goldfish match {
          case Some(goldfishNum) => goldfishNum < 5
          case None              => true
        }
        val treeCheck = sue.trees match {
          case Some(treeNum) => treeNum > 3
          case None          => true
        }
        val carCheck = sue.cars match {
          case Some(carNum) => carNum == 2
          case None         => true
        }
        val perfumeCheck = sue.perfumes match {
          case Some(perfumeNum) => perfumeNum == 1
          case None             => true
        }

        childCheck && catCheck && samoyedCheck && pomeranianCheck && akitaCheck &&
        vizslaCheck && goldfishCheck && treeCheck && carCheck && perfumeCheck
      }
      .head
      .number
    println(s"Part 2: $part2")
  }

}
