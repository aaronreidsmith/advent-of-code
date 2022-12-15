package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day16 extends Solution(2015, 16) {
  type I  = List[Sue]
  type O1 = Int
  type O2 = Int

  private[year2015] case class Sue(
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

  override protected[year2015] def parseInput(file: Source): List[Sue] = {
    val sue         = "^Sue (\\d+): (.*)$".r
    val children    = "^children: (\\d+)$".r
    val cats        = "^cats: (\\d+)$".r
    val samoyeds    = "^samoyeds: (\\d+)$".r
    val pomeranians = "^pomeranians: (\\d+)$".r
    val akitas      = "^akitas: (\\d+)$".r
    val vizslas     = "^vizslas: (\\d+)$".r
    val goldfish    = "^goldfish: (\\d+)$".r
    val trees       = "^trees: (\\d+)$".r
    val cars        = "^cars: (\\d+)$".r
    val perfumes    = "^perfumes: (\\d+)$".r

    file.getLines().foldLeft(List.empty[Sue]) {
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
        newSue :: acc
      case _ => throw new IllegalArgumentException
    }
  }

  override protected[year2015] def part1(sues: List[Sue]): Int = sues
    .filter { sue =>
      val childCheck      = sue.children.fold(true)(_ == 3)
      val catCheck        = sue.cats.fold(true)(_ == 7)
      val samoyedCheck    = sue.samoyeds.fold(true)(_ == 2)
      val pomeranianCheck = sue.pomeranians.fold(true)(_ == 3)
      val akitaCheck      = sue.akitas.fold(true)(_ == 0)
      val vizslaCheck     = sue.vizslas.fold(true)(_ == 0)
      val goldfishCheck   = sue.goldfish.fold(true)(_ == 5)
      val treeCheck       = sue.trees.fold(true)(_ == 3)
      val carCheck        = sue.cars.fold(true)(_ == 2)
      val perfumeCheck    = sue.perfumes.fold(true)(_ == 1)

      childCheck && catCheck && samoyedCheck && pomeranianCheck && akitaCheck &&
      vizslaCheck && goldfishCheck && treeCheck && carCheck && perfumeCheck
    }
    .head
    .number

  override protected[year2015] def part2(sues: List[Sue]): Int = sues
    .filter { sue =>
      val childCheck      = sue.children.fold(true)(_ == 3)
      val catCheck        = sue.cats.fold(true)(_ > 7)
      val samoyedCheck    = sue.samoyeds.fold(true)(_ == 2)
      val pomeranianCheck = sue.pomeranians.fold(true)(_ < 3)
      val akitaCheck      = sue.akitas.fold(true)(_ == 0)
      val vizslaCheck     = sue.vizslas.fold(true)(_ == 0)
      val goldfishCheck   = sue.goldfish.fold(true)(_ < 5)
      val treeCheck       = sue.trees.fold(true)(_ > 3)
      val carCheck        = sue.cars.fold(true)(_ == 2)
      val perfumeCheck    = sue.perfumes.fold(true)(_ == 1)

      childCheck && catCheck && samoyedCheck && pomeranianCheck && akitaCheck &&
      vizslaCheck && goldfishCheck && treeCheck && carCheck && perfumeCheck
    }
    .head
    .number
}
