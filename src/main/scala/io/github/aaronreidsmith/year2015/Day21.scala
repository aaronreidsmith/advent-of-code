package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day21 extends Solution {
  type I  = Boss
  type O1 = Int
  type O2 = Int

  enum Weapon(val cost: Int, val damage: Int) {
    case Dagger     extends Weapon(8, 4)
    case ShortSword extends Weapon(10, 5)
    case WarHammer  extends Weapon(25, 6)
    case Longsword  extends Weapon(40, 7)
    case GreatAxe   extends Weapon(74, 8)
  }

  enum Armor(val cost: Int, val armor: Int) {
    case Leather    extends Armor(13, 1)
    case ChainMail  extends Armor(31, 2)
    case SplintMail extends Armor(53, 3)
    case BandedMail extends Armor(75, 4)
    case PlateMail  extends Armor(102, 5)
  }

  enum Ring(val cost: Int, val damage: Int, val armor: Int) {
    case DamagePlusOne   extends Ring(25, 1, 0)
    case DamagePlusTwo   extends Ring(50, 2, 0)
    case DamagePlusThree extends Ring(100, 3, 0)
    case ArmorPlusOne    extends Ring(20, 0, 1)
    case ArmorPlusTwo    extends Ring(40, 0, 2)
    case ArmorPlusThree  extends Ring(80, 0, 3)
  }

  // Can't use an enum here because the
  sealed trait Character {
    val armor: Int
    val attack: Int
    val hp: Int

    def beAttacked(damage: Int): Character

    val isAlive: Boolean = hp > 0

    def attack(that: Character): Character = that.beAttacked(attack)
  }

  case class Player(
      weapon: Weapon,
      maybeArmor: Option[Armor],
      rings: List[Ring],
      override val hp: Int = 100
  ) extends Character {
    override val attack: Int = weapon.damage + rings.foldLeft(0)(_ + _.damage)
    override val armor: Int  = maybeArmor.fold(0)(_.armor) + rings.foldLeft(0)(_ + _.armor)

    override def beAttacked(damage: Int): Character = this.copy(hp = this.hp - math.max(1, damage - armor))

    val itemCost: Int = weapon.cost + maybeArmor.fold(0)(_.cost) + rings.foldLeft(0)(_ + _.cost)
  }

  case class Boss(override val hp: Int, override val armor: Int, override val attack: Int) extends Character {
    override def beAttacked(damage: Int): Character = this.copy(hp = this.hp - math.max(1, damage - armor))
  }

  private val weapons = Weapon.values.toList
  private val armors  = Armor.values.toList.map(Option(_)) :+ None
  private val rings   = Ring.values.toList

  override def parseInput(file: Source): Boss = {
    val Array(bossHp, bossDamage, bossArmor, _*) =
      file.mkString.trim.split('\n').map(_.split(": ").last.toInt): @unchecked
    Boss(bossHp, bossArmor, bossDamage)
  }

  override def part1(boss: Boss): Int = {
    for {
      weapon <- weapons
      armor  <- armors
      rings  <- List.empty[Ring] :: rings.combinations(1).toList ::: rings.combinations(2).toList
      player = Player(weapon, armor, rings)
      if playerWins(player, boss)
    } yield player.itemCost
  }.min

  override def part2(boss: Boss): Int = {
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
