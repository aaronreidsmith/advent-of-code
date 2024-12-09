package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day09 extends Solution {
  type I  = Vector[Block]
  type O1 = Long
  type O2 = Long

  enum Block {
    case File(id: Int, size: Int)
    case Free(size: Int)
  }

  override def parseInput(file: Source): Vector[Block] = {
    file.mkString.zipWithIndex.map {
      case (char, i) if i % 2 == 0 => Block.File(i / 2, char.asDigit)
      case (char, _) => Block.Free(char.asDigit)
    }.toVector
  }

  override def part1(input: Vector[Block]): Long = {
    @tailrec
    def helper(disk: Vector[Block], acc: Vector[Block]): Long = {
      disk match {
        case (file: Block.File) +: remaining                   => helper(remaining, file +: acc)
        case (remaining @ Block.Free(_) +: _) :+ Block.Free(_) => helper(remaining, acc)
        case Block.Free(freeSize) +: remaining :+ (file @ Block.File(id, fileSize)) =>
          if (freeSize == fileSize) {
            helper(remaining, file +: acc)
          } else if (freeSize > fileSize) {
            helper(Block.Free(freeSize - fileSize) +: remaining, file +: acc)
          } else {
            helper(remaining :+ Block.File(id, fileSize - freeSize), Block.File(id, freeSize) +: acc)
          }
        case _ => checksum(acc.reverse)
      }
    }

    helper(input, Vector.empty)
  }

  override def part2(input: Vector[Block]): Long = {
    @tailrec
    def helper(disk: Vector[Block], acc: Vector[Block]): Long = {
      disk match {
        case remaining :+ (free: Block.Free) => helper(remaining, free +: acc)
        case remaining :+ (file @ Block.File(id, fileSize)) =>
          val moveIndex = remaining.indexWhere {
            case Block.Free(freeSize) => freeSize >= fileSize
            case _                    => false
          }
          if (moveIndex >= 0) {
            val (before, Block.Free(freeSize) +: after) = remaining.splitAt(moveIndex): @unchecked
            val replace = if (freeSize == fileSize) Vector(file) else Vector(file, Block.Free(freeSize - fileSize))
            helper(before ++ replace ++ after, Block.Free(fileSize) +: acc)
          } else {
            helper(remaining, file +: acc)
          }
        case _ => checksum(acc)
      }
    }

    helper(input, Vector.empty)
  }

  private def checksum(disk: Vector[Block]): Long = {
    disk
      .foldLeft((0L, 0)) {
        case ((acc, i), Block.File(id, size)) => (acc + id * (i until i + size).sum.toLong, i + size)
        case ((acc, i), Block.Free(size))     => (acc, i + size)
      }
      ._1
  }
}
