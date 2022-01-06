package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.using

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

// Copied from my Raku solution, so could probably be cleaned up
object Day22 {
  private[year2015] case class GameState(
      bossHp: Int,
      bossAttack: Int,
      playerHp: Int = 50,
      armor: Int = 0,
      playerMana: Int = 500,
      manaSpent: Int = 0,
      shield: Int = 0,
      poison: Int = 0,
      recharge: Int = 0,
      turn: Turn = Player,
      hardMode: Boolean = false
  ) {
    def applyPoison: GameState = if (poison > 0) {
      this.copy(bossHp = bossHp - 3, poison = poison - 1)
    } else {
      this
    }

    def applyRecharge: GameState = if (recharge > 0) {
      this.copy(playerMana = playerMana + 101, recharge = recharge - 1)
    } else {
      this
    }

    def applyShield: GameState = if (shield > 0) {
      this.copy(armor = 7, shield = shield - 1)
    } else {
      this.copy(armor = 0)
    }

    def applyHardMode: GameState = if (hardMode) {
      this.copy(playerHp = playerHp - 1)
    } else {
      this
    }

    def bossTurn: GameState = {
      val damage = (bossAttack - armor).max(1)
      this.copy(playerHp = playerHp - damage)
    }

    def cast(spell: Spell): GameState = {
      val partiallyUpdated = this.copy(
        playerMana = (playerMana - spell.manaCost).max(0),
        manaSpent = manaSpent + spell.manaCost
      )
      spell match {
        case MagicMissile => partiallyUpdated.copy(bossHp = bossHp - 4)
        case Drain        => partiallyUpdated.copy(bossHp = bossHp - 2, playerHp = playerHp + 2)
        case Shield       => partiallyUpdated.copy(armor = 7, shield = 6)
        case Poison       => partiallyUpdated.copy(poison = 6)
        case Recharge     => partiallyUpdated.copy(recharge = 5)
      }
    }

    def nextTurn: GameState = this.copy(turn = turn.next)
  }

  private[year2015] sealed trait Spell { val manaCost: Int }
  private[year2015] case object MagicMissile extends Spell { val manaCost: Int = 53  }
  private[year2015] case object Drain        extends Spell { val manaCost: Int = 73  }
  private[year2015] case object Shield       extends Spell { val manaCost: Int = 113 }
  private[year2015] case object Poison       extends Spell { val manaCost: Int = 173 }
  private[year2015] case object Recharge     extends Spell { val manaCost: Int = 229 }

  private[year2015] sealed trait Turn { val next: Turn }
  private[year2015] case object Boss   extends Turn { val next: Turn = Player }
  private[year2015] case object Player extends Turn { val next: Turn = Boss   }
  private[year2015] type Winner = Turn

  def main(args: Array[String]): Unit = {
    val initialGameState = using("2015/day22.txt")(parseInput)
    println(s"Part 1: ${part1(initialGameState)}")
    println(s"Part 2: ${part2(initialGameState)}")
  }

  private[year2015] def parseInput(file: Source): GameState = {
    val List(bossHp, bossAttack, _*) = file.getLines().toList.map(line => line.filter(_.isDigit).toInt)
    GameState(bossHp, bossAttack)
  }

  private[year2015] def part1(gameState: GameState): Int = {
    val regularMode = gameState.copy(hardMode = false) // Ensure hard mode is disabled
    solution(regularMode, 10000)
  }

  private[year2015] def part2(gameState: GameState): Int = {
    val hardMode = gameState.copy(hardMode = true) // Ensure hard mode is enabled
    solution(hardMode, 100000)
  }

  private def randomSpells(): Seq[Spell] = Random.shuffle(Seq(MagicMissile, Drain, Shield, Poison, Recharge))

  private def solution(gameState: GameState, n: Int): Int = (0 to n).foldLeft(Int.MaxValue) { (currentBest, _) =>
    val (winner, manaSpent) = fight(gameState)
    winner match {
      case Player => currentBest.min(manaSpent)
      case Boss   => currentBest
    }
  }

  @tailrec
  private def fight(gameState: GameState): (Winner, Int) = {
    val updated = gameState.applyShield.applyPoison

    if (updated.bossHp <= 0) {
      (Player, updated.manaSpent)
    } else {
      val recharged = updated.applyRecharge
      recharged.turn match {
        case Player =>
          val withHardModeApplied = recharged.applyHardMode
          if (withHardModeApplied.playerHp <= 0) {
            (Boss, withHardModeApplied.manaSpent)
          } else {
            val spellToCast = {
              for {
                spell <- randomSpells()
                if withHardModeApplied.playerMana >= spell.manaCost
              } yield {
                spell match {
                  case Shield if withHardModeApplied.shield > 0     => None
                  case Poison if withHardModeApplied.poison > 0     => None
                  case Recharge if withHardModeApplied.recharge > 0 => None
                  case _                                            => Some(spell)
                }
              }
            }.flatten.headOption

            spellToCast match {
              case Some(spell) =>
                val withSpellCast = withHardModeApplied.cast(spell)
                if (withSpellCast.bossHp <= 0) {
                  (Player, withSpellCast.manaSpent)
                } else {
                  fight(withSpellCast.nextTurn)
                }
              case None => (Boss, withHardModeApplied.manaSpent)
            }
          }
        case Boss =>
          val withBossAttack = recharged.bossTurn
          if (withBossAttack.playerHp <= 0) {
            (Boss, withBossAttack.manaSpent)
          } else {
            fight(withBossAttack.nextTurn)
          }
      }
    }
  }
}