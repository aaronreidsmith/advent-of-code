package io.github.aaronreidsmith.year2018

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Ordered.orderingToOrdered

// Adapted from https://git.io/Jn2WO. Works for part 2 but not part 1??? Had to use this for part 1:
// https://www.reddit.com/r/adventofcode/comments/a6chwa/2018_day_15_solutions/ebtwcqr
object Day15 {
  private case class Point(row: Int, col: Int) extends Ordered[Point] {
    val position: (Int, Int) = (row, col)

    def reachable: List[Point] = List(
      this.copy(col = col - 1),
      this.copy(col = col + 1),
      this.copy(row = row - 1),
      this.copy(row = row + 1)
    )

    override def compare(that: Point): Int = this.position compare that.position
  }
  private case class Creature(position: Point, isGoblin: Boolean, health: Int = 200) extends Ordered[Creature] {
    override def compare(that: Creature): Int = this.position compare that.position
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2018/day15.txt")
    val (grid, entities) =
      input.getLines().zipWithIndex.foldLeft((Vector.empty[Vector[Boolean]], List.empty[Creature])) {
        case ((gridAcc, entityAcc), (line, row)) =>
          val (newGridEntries, newEntities) =
            line.zipWithIndex.foldLeft((Vector.empty[Boolean], List.empty[Creature])) {
              case ((newGridAcc, newCreatureAcc), (char, col)) =>
                char match {
                  case '#' => (newGridAcc :+ false, newCreatureAcc)
                  case '.' => (newGridAcc :+ true, newCreatureAcc)
                  case 'E' => (newGridAcc :+ true, newCreatureAcc :+ Creature(Point(row, col), isGoblin = false))
                  case 'G' => (newGridAcc :+ true, newCreatureAcc :+ Creature(Point(row, col), isGoblin = true))
                  case _   => throw new IllegalArgumentException
                }
            }
          (gridAcc :+ newGridEntries, entityAcc ++ newEntities)
      }
    input.close()

    lazy val (part1, _) = solution()
    println(s"Part 1: $part1")
    lazy val part2 = Stream
      .from(4)
      .map(attackPower => solution(elvesAttack = attackPower))
      .collectFirst {
        case (outcome, noElvesDied) if noElvesDied => outcome
      }
      .getOrElse(-1)
    println(s"Part 2: $part2")

    @tailrec
    def solution(
        waiting: List[Creature] = Nil,
        played: List[Creature] = entities,
        rounds: Int = 0,
        elvesAttack: Int = 3
    ): (Int, Boolean) = {
      // Defined first because used in below helpers
      val creatures = waiting ++ played

      // Helper functions
      def noWall(pos: Point): Boolean =
        pos.row >= 0 &&
          pos.row < grid.size &&
          pos.col >= 0 &&
          pos.col < grid.head.size &&
          grid(pos.row)(pos.col)

      def canWalk(pos: Point): Boolean = noWall(pos) && creatures.forall(_.position != pos)

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
                  acc ++ point.reachable.collect {
                    case reachable if canWalk(reachable) => reachable
                  }
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
        (outcome, entities.count(!_.isGoblin) == creatures.count(!_.isGoblin))
      } else {
        waiting match {
          case currentCreature :: remainingCreatures =>
            val orientations = currentCreature.position.reachable.filter(noWall)
            val currentEnemies = creatures.filter { creature =>
              creature != currentCreature && creature.isGoblin != currentCreature.isGoblin
            }
            val currentCreatureUpdated = if (!currentEnemies.exists(enemy => orientations.contains(enemy.position))) {
              val targets = orientations
                .withFilter(canWalk)
                .map { orientation =>
                  val distances = currentEnemies.foldLeft(List.empty[Int]) { (acc, enemy) =>
                    val reachable = enemy.position.reachable
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
                  .minBy(_.position)
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
              .sortBy(creature => (creature.health, creature.position.position))
            val maybeTarget = enemies.find { enemy =>
              currentCreatureUpdated.position.reachable.filter(noWall).contains(enemy.position)
            }

            val (newWaiting, newPlayed) = maybeTarget match {
              case Some(target) =>
                val attackPower   = if (currentCreatureUpdated.isGoblin) 3 else elvesAttack
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

            solution(newWaiting, newPlayed, rounds, elvesAttack)

          case Nil => solution(played.sorted, Nil, rounds + 1, elvesAttack)
        }
      }
    }
  }
}
