package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

// Copied from my Raku solution, so could probably be cleaned up
object Day22 extends Solution {
  type I  = GameState
  type O1 = Int
  type O2 = Int

  case class GameState(
      bossHp: Int,
      bossAttack: Int,
      playerHp: Int = 50,
      armor: Int = 0,
      playerMana: Int = 500,
      manaSpent: Int = 0,
      shield: Int = 0,
      poison: Int = 0,
      recharge: Int = 0,
      turn: Turn = Turn.Player,
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
        case Spell.MagicMissile => partiallyUpdated.copy(bossHp = bossHp - 4)
        case Spell.Drain        => partiallyUpdated.copy(bossHp = bossHp - 2, playerHp = playerHp + 2)
        case Spell.Shield       => partiallyUpdated.copy(armor = 7, shield = 6)
        case Spell.Poison       => partiallyUpdated.copy(poison = 6)
        case Spell.Recharge     => partiallyUpdated.copy(recharge = 5)
      }
    }

    def nextTurn: GameState = this.copy(turn = turn.next)
  }

  enum Spell(val manaCost: Int) {
    case MagicMissile extends Spell(53)
    case Drain        extends Spell(73)
    case Shield       extends Spell(113)
    case Poison       extends Spell(173)
    case Recharge     extends Spell(229)
  }

  enum Turn {
    case Boss, Player

    def next: Turn = this match {
      case Boss   => Player
      case Player => Boss
    }
  }
  type Winner = Turn

  override def parseInput(file: Source): GameState = {
    val List(bossHp, bossAttack, _*) = file.getLines().toList.map(line => line.filter(_.isDigit).toInt): @unchecked
    GameState(bossHp, bossAttack)
  }

  override def part1(gameState: GameState): Int = {
    val regularMode = gameState.copy(hardMode = false) // Ensure hard mode is disabled
    solution(regularMode, 10000)
  }

  override def part2(gameState: GameState): Int = {
    val hardMode = gameState.copy(hardMode = true) // Ensure hard mode is enabled
    solution(hardMode, 100000)
  }

  private def randomSpells(): Seq[Spell] = Random.shuffle(Spell.values.toSeq)

  private def solution(gameState: GameState, n: Int): Int = (0 to n).foldLeft(Int.MaxValue) { (currentBest, _) =>
    val (winner, manaSpent) = fight(gameState)
    winner match {
      case Turn.Player => currentBest.min(manaSpent)
      case Turn.Boss   => currentBest
    }
  }

  @tailrec
  private def fight(gameState: GameState): (Winner, Int) = {
    val updated = gameState.applyShield.applyPoison

    if (updated.bossHp <= 0) {
      (Turn.Player, updated.manaSpent)
    } else {
      val recharged = updated.applyRecharge
      recharged.turn match {
        case Turn.Player =>
          val withHardModeApplied = recharged.applyHardMode
          if (withHardModeApplied.playerHp <= 0) {
            (Turn.Boss, withHardModeApplied.manaSpent)
          } else {
            val spellToCast = {
              for {
                spell <- randomSpells()
                if withHardModeApplied.playerMana >= spell.manaCost
              } yield {
                spell match {
                  case Spell.Shield if withHardModeApplied.shield > 0     => None
                  case Spell.Poison if withHardModeApplied.poison > 0     => None
                  case Spell.Recharge if withHardModeApplied.recharge > 0 => None
                  case _                                                  => Some(spell)
                }
              }
            }.flatten.headOption

            spellToCast match {
              case Some(spell) =>
                val withSpellCast = withHardModeApplied.cast(spell)
                if (withSpellCast.bossHp <= 0) {
                  (Turn.Player, withSpellCast.manaSpent)
                } else {
                  fight(withSpellCast.nextTurn)
                }
              case None => (Turn.Boss, withHardModeApplied.manaSpent)
            }
          }
        case Turn.Boss =>
          val withBossAttack = recharged.bossTurn
          if (withBossAttack.playerHp <= 0) {
            (Turn.Boss, withBossAttack.manaSpent)
          } else {
            fight(withBossAttack.nextTurn)
          }
      }
    }
  }
}
