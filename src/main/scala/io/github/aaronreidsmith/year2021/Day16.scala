package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day16 extends Solution {
  type I  = Packet
  type O1 = Int
  type O2 = Long

  private type Bits = List[Boolean]
  private implicit class BitsOps(bits: Bits) {
    def toInt: Int   = toLong.toInt
    def toLong: Long = bits.foldLeft(0L)((acc, bit) => (acc << 1) | (if (bit) 1 else 0))
  }

  object Packet {
    private val hexMap = Map(
      '0' -> List(false, false, false, false),
      '1' -> List(false, false, false, true),
      '2' -> List(false, false, true, false),
      '3' -> List(false, false, true, true),
      '4' -> List(false, true, false, false),
      '5' -> List(false, true, false, true),
      '6' -> List(false, true, true, false),
      '7' -> List(false, true, true, true),
      '8' -> List(true, false, false, false),
      '9' -> List(true, false, false, true),
      'A' -> List(true, false, true, false),
      'B' -> List(true, false, true, true),
      'C' -> List(true, true, false, false),
      'D' -> List(true, true, false, true),
      'E' -> List(true, true, true, false),
      'F' -> List(true, true, true, true)
    )

    def apply(hex: String): Packet = {
      val bits        = hex.flatMap(hexMap).toList
      val (packet, _) = parsePacket(bits)
      packet
    }

    private def parseLiteral(bits: Bits): (Bits, Bits) = {
      val (prefix :: groupBits, remaining1) = bits.splitAt(5)
      if (prefix) {
        val (tailBits, remaining2) = parseLiteral(remaining1)
        (groupBits ++ tailBits, remaining2)
      } else {
        (groupBits, remaining1)
      }
    }

    private def parseSubPacketsLength(bits: Bits): List[Packet] = bits match {
      case Nil => Nil
      case _ =>
        val (packet, remaining) = parsePacket(bits)
        val packets             = parseSubPacketsLength(remaining)
        packet :: packets
    }

    private def parseSubPacketsNumber(bits: Bits, n: Int): (List[Packet], Bits) = if (n == 0) {
      (Nil, bits)
    } else {
      val (packet, remaining1)  = parsePacket(bits)
      val (packets, remaining2) = parseSubPacketsNumber(remaining1, n - 1)
      (packet :: packets, remaining2)
    }

    private def parsePacket(bits: Bits): (Packet, Bits) = {
      val (versionBits, remaining1) = bits.splitAt(3)
      val version                   = versionBits.toInt
      val (typeIdBits, remaining2)  = remaining1.splitAt(3)
      val typeId                    = typeIdBits.toInt
      typeId match {
        case 4 =>
          val (valueBits, remaining3) = parseLiteral(remaining2)
          val value                   = valueBits.toLong
          (Literal(version, value), remaining3)
        case _ =>
          val lengthType :: remaining3 = remaining2
          if (lengthType) {
            val (numberBits, remaining4) = remaining3.splitAt(11)
            val number                   = numberBits.toInt
            val (subpackets, remaining5) = parseSubPacketsNumber(remaining4, number)
            (Operator(version, typeId, subpackets), remaining5)
          } else {
            val (lengthBits, remaining4)    = remaining3.splitAt(15)
            val length                      = lengthBits.toInt
            val (subpacketBits, remaining5) = remaining4.splitAt(length)
            val subpackets                  = parseSubPacketsLength(subpacketBits)
            (Operator(version, typeId, subpackets), remaining5)
          }
      }
    }
  }

  sealed trait Packet { val version: Int }
  case class Literal(version: Int, value: Long)                            extends Packet
  case class Operator(version: Int, typeId: Int, subPackets: List[Packet]) extends Packet

  override def parseInput(file: Source): Packet = Packet(file.mkString.trim)

  override def part1(packet: Packet): Int = packet match {
    case Literal(version, _)              => version
    case Operator(version, _, subPackets) => version + subPackets.foldLeft(0)((acc, sub) => acc + part1(sub))
  }

  override def part2(packet: Packet): Long = packet match {
    case Literal(_, value) => value
    case Operator(_, typeId, subPackets) =>
      val subValues = subPackets.map(part2)
      typeId match {
        case 0 => subValues.sum
        case 1 => subValues.product
        case 2 => subValues.min
        case 3 => subValues.max
        case 5 =>
          val a :: b :: _ = subValues
          if (a > b) 1 else 0
        case 6 =>
          val a :: b :: _ = subValues
          if (a < b) 1 else 0
        case 7 =>
          val a :: b :: _ = subValues
          if (a == b) 1 else 0
        case _ => throw new IllegalArgumentException
      }
  }
}
