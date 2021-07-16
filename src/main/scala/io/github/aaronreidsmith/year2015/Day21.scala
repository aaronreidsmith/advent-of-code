package io.github.aaronreidsmith.year2015

import scala.annotation.tailrec
import scala.io.Source

object Day21 {
  sealed trait Item {
    val cost: Int
    val damage: Int
    val armor: Int
  }

  // Weapons
  sealed trait Weapon extends Item
  case object Dagger extends Weapon {
    override val cost: Int   = 8
    override val damage: Int = 4
    override val armor: Int  = 0
  }
  case object Shortsword extends Weapon {
    override val cost: Int   = 10
    override val damage: Int = 5
    override val armor: Int  = 0
  }
  case object Warhammer extends Weapon {
    override val cost: Int   = 25
    override val damage: Int = 6
    override val armor: Int  = 0
  }
  case object Longsword extends Weapon {
    override val cost: Int   = 40
    override val damage: Int = 7
    override val armor: Int  = 0
  }
  case object Greataxe extends Weapon {
    override val cost: Int   = 74
    override val damage: Int = 8
    override val armor: Int  = 0
  }

  // Armor
  sealed trait Armor extends Item
  case object Leather extends Armor {
    override val cost: Int   = 13
    override val damage: Int = 0
    override val armor: Int  = 1
  }
  case object Chainmail extends Armor {
    override val cost: Int   = 31
    override val damage: Int = 0
    override val armor: Int  = 2
  }
  case object Splintmail extends Armor {
    override val cost: Int   = 53
    override val damage: Int = 0
    override val armor: Int  = 3
  }
  case object Bandedmail extends Armor {
    override val cost: Int   = 75
    override val damage: Int = 0
    override val armor: Int  = 4
  }
  case object Platemail extends Armor {
    override val cost: Int   = 102
    override val damage: Int = 0
    override val armor: Int  = 5
  }

  // Rings
  sealed trait Ring extends Item
  case object DamagePlusOne extends Ring {
    override val cost: Int   = 25
    override val damage: Int = 1
    override val armor: Int  = 0
  }
  case object DamagePlusTwo extends Ring {
    override val cost: Int   = 50
    override val damage: Int = 2
    override val armor: Int  = 0
  }
  case object DamagePlusThree extends Ring {
    override val cost: Int   = 100
    override val damage: Int = 3
    override val armor: Int  = 0
  }
  case object DefensePlusOne extends Ring {
    override val cost: Int   = 20
    override val damage: Int = 0
    override val armor: Int  = 1
  }
  case object DefensePlusTwo extends Ring {
    override val cost: Int   = 40
    override val damage: Int = 0
    override val armor: Int  = 2
  }
  case object DefensePlusThree extends Ring {
    override val cost: Int   = 80
    override val damage: Int = 0
    override val armor: Int  = 3
  }

  sealed trait Character {
    val armor: Int
    val attack: Int
    val hp: Int

    def beAttacked(damage: Int): Character

    val isAlive: Boolean = hp > 0

    def attack(that: Character): Character = that.beAttacked(attack)
  }
  case class Player(weapon: Weapon, maybeArmor: Option[Armor], rings: List[Ring], override val hp: Int = 100)
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

  case class Boss(override val hp: Int, override val armor: Int, override val attack: Int) extends Character {
    override def beAttacked(damage: Int): Character = this.copy(hp = this.hp - math.max(1, damage - armor))
  }

  @tailrec
  private def playerWins(attacker: Character, defender: Character): Boolean = {
    val updatedDefender = attacker.attack(defender)
    updatedDefender match {
      case player: Player if !player.isAlive => false
      case boss: Boss if !boss.isAlive       => true
      case _                                 => playerWins(updatedDefender, attacker)
    }
  }

  def main(args: Array[String]): Unit = {
    val input                                    = Source.fromResource("2015/day21.txt")
    val Array(bossHp, bossDamage, bossArmor, _*) = input.mkString.split('\n').map(_.split(": ").last.toInt)
    input.close()

    val boss = Boss(bossHp, bossArmor, bossDamage)

    val weapons = List(Dagger, Shortsword, Warhammer, Longsword, Greataxe)
    val armors  = List(Some(Leather), Some(Chainmail), Some(Splintmail), Some(Bandedmail), Some(Platemail), None)
    val rings   = List(DamagePlusOne, DamagePlusTwo, DamagePlusThree, DefensePlusOne, DefensePlusTwo, DefensePlusThree)

    val part1 = (
      for {
        weapon <- weapons
        armor  <- armors
        rings  <- List.empty[Ring] :: rings.combinations(1).toList ::: rings.combinations(2).toList
        player = Player(weapon, armor, rings)
        if playerWins(player, boss)
      } yield player.itemCost
    ).min
    println(s"Part 1: $part1")

    val part2 = (
      for {
        weapon <- weapons
        armor  <- armors
        rings  <- List.empty[Ring] :: rings.combinations(1).toList ::: rings.combinations(2).toList
        player = Player(weapon, armor, rings)
        if !playerWins(player, boss)
      } yield player.itemCost
    ).max
    println(s"Part 2: $part2")
  }

}