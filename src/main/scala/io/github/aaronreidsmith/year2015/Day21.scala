package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{Solution, using}

import scala.annotation.tailrec
import scala.io.Source

object Day21 extends Solution {
  type I  = Boss
  type O1 = Int
  type O2 = Int

  private sealed trait Item {
    val cost: Int
    val damage: Int
    val armor: Int
  }

  // Weapons
  private sealed trait Weapon extends Item
  private case object Dagger extends Weapon {
    override val cost: Int   = 8
    override val damage: Int = 4
    override val armor: Int  = 0
  }
  private case object Shortsword extends Weapon {
    override val cost: Int   = 10
    override val damage: Int = 5
    override val armor: Int  = 0
  }
  private case object Warhammer extends Weapon {
    override val cost: Int   = 25
    override val damage: Int = 6
    override val armor: Int  = 0
  }
  private case object Longsword extends Weapon {
    override val cost: Int   = 40
    override val damage: Int = 7
    override val armor: Int  = 0
  }
  private case object Greataxe extends Weapon {
    override val cost: Int   = 74
    override val damage: Int = 8
    override val armor: Int  = 0
  }

  // Armor
  private sealed trait Armor extends Item
  private case object Leather extends Armor {
    override val cost: Int   = 13
    override val damage: Int = 0
    override val armor: Int  = 1
  }
  private case object Chainmail extends Armor {
    override val cost: Int   = 31
    override val damage: Int = 0
    override val armor: Int  = 2
  }
  private case object Splintmail extends Armor {
    override val cost: Int   = 53
    override val damage: Int = 0
    override val armor: Int  = 3
  }
  private case object Bandedmail extends Armor {
    override val cost: Int   = 75
    override val damage: Int = 0
    override val armor: Int  = 4
  }
  private case object Platemail extends Armor {
    override val cost: Int   = 102
    override val damage: Int = 0
    override val armor: Int  = 5
  }

  // Rings
  private sealed trait Ring extends Item
  private case object DamagePlusOne extends Ring {
    override val cost: Int   = 25
    override val damage: Int = 1
    override val armor: Int  = 0
  }
  private case object DamagePlusTwo extends Ring {
    override val cost: Int   = 50
    override val damage: Int = 2
    override val armor: Int  = 0
  }
  private case object DamagePlusThree extends Ring {
    override val cost: Int   = 100
    override val damage: Int = 3
    override val armor: Int  = 0
  }
  private case object DefensePlusOne extends Ring {
    override val cost: Int   = 20
    override val damage: Int = 0
    override val armor: Int  = 1
  }
  private case object DefensePlusTwo extends Ring {
    override val cost: Int   = 40
    override val damage: Int = 0
    override val armor: Int  = 2
  }
  private case object DefensePlusThree extends Ring {
    override val cost: Int   = 80
    override val damage: Int = 0
    override val armor: Int  = 3
  }

  private[year2015] sealed trait Character {
    val armor: Int
    val attack: Int
    val hp: Int

    def beAttacked(damage: Int): Character

    val isAlive: Boolean = hp > 0

    def attack(that: Character): Character = that.beAttacked(attack)
  }
  private case class Player(weapon: Weapon, maybeArmor: Option[Armor], rings: List[Ring], override val hp: Int = 100)
      extends Character {
    override val attack: Int = weapon.damage + rings.foldLeft(0)(_ + _.damage)
    override val armor: Int = {
      val baseArmor = maybeArmor match {
        case Some(a) => a.armor
        case None    => 0
      }
      baseArmor + rings.foldLeft(0)(_ + _.armor)
    }

    override def beAttacked(damage: Int): Character = this.copy(hp = this.hp - math.max(1, damage - armor))

    val itemCost: Int = {
      val armorCost = maybeArmor match {
        case Some(a) => a.cost
        case None    => 0
      }
      weapon.cost + armorCost + rings.foldLeft(0)(_ + _.cost)
    }
  }

  private[year2015] case class Boss(override val hp: Int, override val armor: Int, override val attack: Int)
      extends Character {
    override def beAttacked(damage: Int): Character = this.copy(hp = this.hp - math.max(1, damage - armor))
  }

  private val weapons = List(Dagger, Shortsword, Warhammer, Longsword, Greataxe)
  private val armors  = List(Some(Leather), Some(Chainmail), Some(Splintmail), Some(Bandedmail), Some(Platemail), None)
  private val rings =
    List(DamagePlusOne, DamagePlusTwo, DamagePlusThree, DefensePlusOne, DefensePlusTwo, DefensePlusThree)

  def run(): Unit = {
    println("Year 2015, Day 21")
    val boss = using("2015/day21.txt")(parseInput)
    println(s"Part 1: ${part1(boss)}")
    println(s"Part 2: ${part2(boss)}")
    println()
  }

  override protected[year2015] def parseInput(file: Source): Boss = {
    val Array(bossHp, bossDamage, bossArmor, _*) = file.mkString.split('\n').map(_.split(": ").last.toInt)
    Boss(bossHp, bossArmor, bossDamage)
  }

  override protected[year2015] def part1(boss: Boss): Int = {
    for {
      weapon <- weapons
      armor  <- armors
      rings  <- List.empty[Ring] :: rings.combinations(1).toList ::: rings.combinations(2).toList
      player = Player(weapon, armor, rings)
      if playerWins(player, boss)
    } yield player.itemCost
  }.min

  override protected[year2015] def part2(boss: Boss): Int = {
    for {
      weapon <- weapons
      armor  <- armors
      rings  <- List.empty[Ring] :: rings.combinations(1).toList ::: rings.combinations(2).toList
      player = Player(weapon, armor, rings)
      if !playerWins(player, boss)
    } yield player.itemCost
  }.max

  @tailrec
  private def playerWins(attacker: Character, defender: Character): Boolean = {
    val updatedDefender = attacker.attack(defender)
    updatedDefender match {
      case player: Player if !player.isAlive => false
      case boss: Boss if !boss.isAlive       => true
      case _                                 => playerWins(updatedDefender, attacker)
    }
  }
}
