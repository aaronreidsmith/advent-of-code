package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day20 extends Solution {
  type I  = List[Particle]
  type O1 = Int
  type O2 = Int

  sealed trait Delta {
    def increment(xDelta: Int, yDelta: Int, zDelta: Int): Delta
  }
  case class Position(x: Int, y: Int, z: Int) extends Delta {
    lazy val distance: Int                                         = math.abs(x) + math.abs(y) + math.abs(z)
    def increment(xDelta: Int, yDelta: Int, zDelta: Int): Position = Position(x + xDelta, y + yDelta, z + zDelta)
  }
  case class Velocity(x: Int, y: Int, z: Int) extends Delta {
    def increment(xDelta: Int, yDelta: Int, zDelta: Int): Velocity = Velocity(x + xDelta, y + yDelta, z + zDelta)
  }
  case class Acceleration(x: Int, y: Int, z: Int)
  case class Particle(id: Int, position: Position, velocity: Velocity, acceleration: Acceleration) {
    def next: Particle = {
      val newVelocity = velocity.increment(acceleration.x, acceleration.y, acceleration.z)
      val newPosition = position.increment(newVelocity.x, newVelocity.y, newVelocity.z)
      Particle(id, newPosition, newVelocity, acceleration)
    }
  }

  override def parseInput(file: Source): List[Particle] = {
    val particle = """^p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>""".r
    file.getLines().zipWithIndex.toList.collect {
      case (particle(xPos, yPos, zPos, xVel, yVel, zVel, xAcc, yAcc, zAcc), id) =>
        Particle(
          id,
          Position(xPos.toInt, yPos.toInt, zPos.toInt),
          Velocity(xVel.toInt, yVel.toInt, zVel.toInt),
          Acceleration(xAcc.toInt, yAcc.toInt, zAcc.toInt)
        )
    }
  }

  override def part1(input: List[Particle]): Int = {
    @tailrec
    def helper(particles: List[Particle], previousMinParticleId: Int = -1, sameMinCount: Int = 0): Int = {
      // If we see the same min point 1000 times in a row, we assume it will always be there
      if (sameMinCount >= 1000) {
        previousMinParticleId
      } else {
        val updatedParticles = particles.map(_.next)
        val minParticle      = updatedParticles.minBy(_.position.distance)
        if (minParticle.id == previousMinParticleId) {
          helper(updatedParticles, minParticle.id, sameMinCount + 1)
        } else {
          helper(updatedParticles, minParticle.id)
        }
      }
    }

    helper(input)
  }

  override def part2(input: List[Particle]): Int = {
    @tailrec
    def helper(particles: List[Particle], previousCollisionCount: Int = -1, zeroCollisionCount: Int = 0): Int = {
      // If we don't see collisions for 1000 iterations, we assume we don't have any more
      if (previousCollisionCount == 0 && zeroCollisionCount >= 1000) {
        particles.size
      } else {
        val updatedParticles = particles.map(_.next)
        val withCollisionsRemoved = updatedParticles.filterNot { particle =>
          updatedParticles.exists(other => other.id != particle.id && other.position == particle.position)
        }
        val difference = updatedParticles.size - withCollisionsRemoved.size
        helper(
          withCollisionsRemoved,
          difference,
          if (previousCollisionCount == 0 && difference == 0) zeroCollisionCount + 1 else 0
        )
      }
    }

    helper(input)
  }
}
