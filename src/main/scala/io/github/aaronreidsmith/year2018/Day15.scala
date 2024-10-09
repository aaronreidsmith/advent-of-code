package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.extensions.toGrid
import io.github.aaronreidsmith.{Grid, Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

// TODO: This runs incredibly slowly for both parts, but it works (at least for my input)
// Adapted from https://git.io/Jn2WO. Works for part 2 but not part 1??? Had to use this for part 1:
// https://www.reddit.com/r/adventofcode/comments/a6chwa/2018_day_15_solutions/ebtwcqr
@Slow(part1 = true, part2 = true)
object Day15 extends Solution {
  type I  = (Grid[Square], List[Creature])
  type O1 = Int
  type O2 = Int

  // Can't use enum here because we use copy on the case class
  sealed trait Square
  case class Creature(position: Point, isGoblin: Boolean, health: Int = 200) extends Square
  case object Empty                                                          extends Square

  override def parseInput(file: Source): (Grid[Square], List[Creature]) = {
    val grid = file.toGrid.collect {
      case (point, '.') => point -> Empty
      case (point, 'E') => point -> Creature(point, isGoblin = false)
      case (point, 'G') => point -> Creature(point, isGoblin = true)
    }
    val initial = grid.collect { case (_, creature: Creature) => creature }.toList.sortBy(_.position)
    (grid, initial)
  }

  override def part1(input: (Grid[Square], List[Creature])): Int = {
    val (grid, initial) = input
    combat(grid, initial, initial)._1
  }

  override def part2(input: (Grid[Square], List[Creature])): Int = {
    val (grid, initial) = input
    Iterator
      .from(4)
      .map(attackPower => combat(grid, initial, initial, elfAttack = attackPower))
      .collectFirst {
        case (outcome, noElvesDied) if noElvesDied => outcome
      }
      .getOrElse(-1)
  }

  @tailrec
  private def combat(
      grid: Grid[Square],
      initial: List[Creature],
      played: List[Creature],
      waiting: List[Creature] = Nil,
      rounds: Int = 0,
      elfAttack: Int = 3
  ): (Int, Boolean) = {
    // Defined first because used in below helpers
    val creatures = waiting ++ played

    // Helper functions
    def canWalk(pos: Point): Boolean = grid.contains(pos) && creatures.forall(_.position != pos)
    def distance(from: Point, to: Point): Option[Int] = {
      @tailrec
      def dfs(sources: Set[Point], visited: Set[Point] = Set(), distance: Int = 0): Option[Int] = {
        if (sources.nonEmpty) {
          if (sources.contains(to)) {
            Some(distance)
          } else {
            val newVisited = visited ++ sources
            val newSources = sources
              .foldLeft(Set.empty[Point]) { (acc, point) =>
                acc ++ point.immediateNeighbors.collect { case reachable if canWalk(reachable) => reachable }
              }
              .diff(newVisited)
            dfs(newSources, newVisited, distance + 1)
          }
        } else {
          None
        }
      }

      dfs(Set(from))
    }

    val finished = {
      val (goblin, elves) = creatures.partition(_.isGoblin)
      goblin.isEmpty || elves.isEmpty
    }

    if (finished) {
      val outcome = creatures.foldLeft(0)(_ + _.health) * (rounds - 1)
      (outcome, initial.count(!_.isGoblin) == creatures.count(!_.isGoblin))
    } else {
      waiting match {
        case currentCreature :: remainingCreatures =>
          val orientations = currentCreature.position.immediateNeighbors.filter(grid.contains)
          val currentEnemies = creatures.filter { creature =>
            creature != currentCreature && creature.isGoblin != currentCreature.isGoblin
          }
          val currentCreatureUpdated = if (!currentEnemies.exists(enemy => orientations.contains(enemy.position))) {
            val targets = orientations
              .withFilter(canWalk)
              .map { orientation =>
                val distances = currentEnemies.foldLeft(List.empty[Int]) { (acc, enemy) =>
                  val reachable = enemy.position.immediateNeighbors
                  val walkable  = reachable.filter(canWalk)
                  acc ++ walkable.flatMap(distance(orientation, _))
                }
                orientation -> (if (distances.nonEmpty) Some(distances.min) else None)
              }
              .toMap

            val distances = targets.values.flatten
            if (distances.nonEmpty) {
              val min = distances.min
              val goto = targets
                .filter {
                  case (_, Some(distance)) => distance == min
                  case _                   => false
                }
                .keys
                .min
              currentCreature.copy(position = goto)
            } else {
              currentCreature
            }
          } else {
            currentCreature
          }

          val enemies = creatures
            .filter { creature =>
              creature != currentCreatureUpdated && creature.isGoblin != currentCreature.isGoblin
            }
            .sortBy(creature => (creature.health, creature.position))
          val maybeTarget = enemies.find { enemy =>
            currentCreatureUpdated.position.immediateNeighbors.filter(grid.contains).contains(enemy.position)
          }

          val (newWaiting, newPlayed) = maybeTarget match {
            case Some(target) =>
              val attackPower   = if (currentCreatureUpdated.isGoblin) 3 else elfAttack
              val victimUpdated = target.copy(health = target.health - attackPower)

              def update(list: List[Creature]): List[Creature] = {
                val updated = list.indexOf(target) match {
                  case -1 => list
                  case i  => list.updated(i, victimUpdated)
                }
                updated.filter(_.health > 0)
              }

              (update(remainingCreatures), update(currentCreatureUpdated :: played))
            case None => (remainingCreatures, currentCreatureUpdated :: played)
          }

          combat(grid, initial, newPlayed, newWaiting, rounds, elfAttack)

        case Nil => combat(grid, initial, Nil, played.sortBy(_.position), rounds + 1, elfAttack)
      }
    }
  }
}
