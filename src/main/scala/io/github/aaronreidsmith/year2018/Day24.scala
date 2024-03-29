package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

// Adapted from https://www.reddit.com/r/adventofcode/comments/a91ysq/comment/ecg6y79
object Day24 extends Solution {
  type I  = List[Group]
  type O1 = Int
  type O2 = Int

  enum GroupType {
    case ImmuneSystem, Infection

    def target: GroupType = this match {
      case ImmuneSystem => Infection
      case Infection    => ImmuneSystem
    }
  }

  case class Group(
      i: Int,
      groupType: GroupType,
      units: Int,
      unitHp: Int,
      attack: Int,
      attackType: String,
      initiative: Int,
      weaknesses: Set[String],
      immunities: Set[String]
  ) {
    def power: Int = units * attack
    def damageTo(defender: Group): Int = if (defender.immunities.contains(attackType)) {
      0
    } else if (defender.weaknesses.contains(attackType)) {
      2 * power
    } else {
      power
    }
  }

  override def parseInput(file: Source): List[Group] = {
    def parseGroup(i: Int, line: String, groupType: GroupType): Group = {
      def parseWeakImmune(string: String): (Set[String], Set[String]) = {
        val weaknessRegex   = """^weak to ([\w, ]+)$""".r
        val immunitiesRegex = """^immune to ([\w, ]+)$""".r
        val defaults        = (Set.empty[String], Set.empty[String])
        Option(string) match {
          case Some(input) =>
            val parts = input.replaceAll("^\\(", "").replaceAll("\\) $", "").split("; ")
            parts.foldLeft(defaults) {
              case ((weaknesses, immunities), weaknessRegex(types)) =>
                (weaknesses ++ types.split(", ").toSet, immunities)
              case ((weaknesses, immunities), immunitiesRegex(types)) =>
                (weaknesses, immunities ++ types.split(", ").toSet)
              case (parsed, _) => parsed
            }
          case None => defaults
        }
      }

      val group =
        """^(\d+) units each with (\d+) hit points (\([^)]+\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
      line match {
        case group(units, unitHp, weakImmune, attackDamage, attackType, initiative) =>
          val (weaknesses, immunities) = parseWeakImmune(weakImmune)
          Group(
            i,
            groupType,
            units.toInt,
            unitHp.toInt,
            attackDamage.toInt,
            attackType,
            initiative.toInt,
            weaknesses,
            immunities
          )
        case _ => throw new IllegalArgumentException
      }
    }
    def parseGroups(block: String): Seq[Group] = {
      val lines     = block.split('\n')
      val groupType = if (lines.head == "Infection:") GroupType.Infection else GroupType.ImmuneSystem
      lines.toSeq.zipWithIndex.tail.map {
        case (line, i) => parseGroup(i, line, groupType)
      }
    }

    file.mkString.split("\n\n").toList.flatMap(parseGroups)
  }

  override def part1(initialGroups: List[Group]): Int = combat(initialGroups) match {
    case Some((_, units)) => units
    case None             => -1
  }

  override def part2(initialGroups: List[Group]): Int = {
    def boostGroups(groups: Seq[Group], boost: Int): Seq[Group] = groups.map { group =>
      group.groupType match {
        case GroupType.ImmuneSystem => group.copy(attack = group.attack + boost)
        case GroupType.Infection    => group
      }
    }

    LazyList
      .from(0)
      .map { boost =>
        val boostedGroups = boostGroups(initialGroups, boost)
        combat(boostedGroups) match {
          case Some((winner, units)) => (winner, units)
          case _                     => (GroupType.Infection, -1)
        }
      }
      .collectFirst {
        case (GroupType.ImmuneSystem, units) => units
      }
      .getOrElse(-1)
  }

  @tailrec
  private def combat(groups: Seq[Group]): Option[(GroupType, Int)] = {
    val remainingGroupTypes = groups.map(_.groupType)
    if (remainingGroupTypes.contains(GroupType.Infection) && remainingGroupTypes.contains(GroupType.ImmuneSystem)) {
      val targets   = targetSelectionPhase(groups)
      val newGroups = attackingPhase(groups, targets)
      if (newGroups == groups) {
        None
      } else {
        combat(newGroups)
      }
    } else {
      Some((groups.head.groupType, groups.foldLeft(0)(_ + _.units)))
    }
  }

  private def targetSelectionPhase(groups: Seq[Group]): Map[Group, Group] = {
    @tailrec
    def helper(
        choosingGroups: List[Group],
        attackableGroups: Set[Group],
        targets: Map[Group, Group] = Map.empty
    ): Map[Group, Group] = choosingGroups match {
      case Nil => targets
      case attackingGroup :: tail =>
        val enemyArmy = attackableGroups.filter(_.groupType == attackingGroup.groupType.target)
        if (enemyArmy.nonEmpty) {
          val target = enemyArmy.maxBy(defendingGroup =>
            (attackingGroup.damageTo(defendingGroup), defendingGroup.power, defendingGroup.initiative)
          )
          if (attackingGroup.damageTo(target) > 0) {
            helper(tail, attackableGroups - target, targets + (attackingGroup -> target))
          } else {
            helper(tail, attackableGroups, targets)
          }
        } else {
          helper(tail, attackableGroups, targets)
        }
    }

    val choosingGroups = groups.sortBy(group => (-group.power, -group.initiative))
    helper(choosingGroups.toList, groups.toSet)
  }

  private def attackingPhase(groups: Seq[Group], targets: Map[Group, Group]): Seq[Group] = {
    @tailrec
    def helper(attackingGroups: List[Group], targets: Map[Group, Group], groups: Set[Group]): Seq[Group] = {
      attackingGroups match {
        case Nil => groups.toSeq
        case attackingGroup :: tail =>
          val defendingGroup    = targets(attackingGroup)
          val damage            = attackingGroup.damageTo(defendingGroup)
          val killedUnits       = damage / defendingGroup.unitHp
          val newDefendingGroup = defendingGroup.copy(units = defendingGroup.units - killedUnits)

          if (newDefendingGroup.units > 0) {
            val newGroups = groups - defendingGroup + newDefendingGroup
            val newTargets = if (targets.contains(defendingGroup)) {
              targets - defendingGroup + (newDefendingGroup -> targets(defendingGroup))
            } else {
              targets
            }
            val newTail = tail.map(group => if (group == defendingGroup) newDefendingGroup else group)
            helper(newTail, newTargets, newGroups)
          } else {
            val newGroups  = groups - defendingGroup
            val newTargets = targets - defendingGroup
            val newTail    = tail.diff(List(defendingGroup))
            helper(newTail, newTargets, newGroups)
          }
      }
    }

    val attackingGroups = targets.keys.toList.sortBy(group => -group.initiative)
    helper(attackingGroups, targets, groups.toSet)
  }
}
