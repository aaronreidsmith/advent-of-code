package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day19 extends Solution {
  type I  = (Map[String, Workflow], List[Part])
  type O1 = Int
  type O2 = Long

  enum Category {
    case X, M, A, S
  }
  object Category {
    def apply(char: Char): Category = char match {
      case 'x' => X
      case 'm' => M
      case 'a' => A
      case 's' => S
      case _   => throw new IllegalArgumentException
    }
  }

  enum Operator {
    case LT, GT
  }
  object Operator {
    def apply(char: Char): Operator = char match {
      case '<' => LT
      case '>' => GT
      case _   => throw new IllegalArgumentException
    }
  }

  case class Rule(category: Category, op: Operator, value: Int, target: String) {
    def evaluate(part: Part): Boolean = {
      val partValue = part(category)
      op match {
        case Operator.LT => partValue < value
        case Operator.GT => partValue > value
      }
    }
  }

  case class Workflow(rules: List[Rule], default: String) {
    def destination(part: Part): String = rules
      .collectFirst {
        case rule if rule.evaluate(part) => rule.target
      }
      .getOrElse(default)
  }

  type Part = Map[Category, Int]

  case class Span(start: Int, end: Int) {
    def size: Int                   = end - start + 1
    def split(i: Int): (Span, Span) = (Span(start, i - 1), Span(i, end))
  }

  override def parseInput(file: Source): (Map[String, Workflow], List[Part]) = {
    val Array(workflowsRaw, partsRaw, _*) = file.mkString.trim.split("\n\n"): @unchecked

    val workflowRegex = """^([a-z]+)\{((?:[xmas][<>]\d+:[a-zA-Z]+,)+)([a-zA-Z]+)}$""".r
    val workflows = workflowsRaw.split('\n').foldLeft(Map.empty[String, Workflow]) {
      case (acc, workflowRegex(name, rulesRaw, default)) =>
        val rules = rulesRaw.init.split(',').toList.map { rule =>
          val Array(comparison, target, _*) = rule.split(':'): @unchecked
          val category                      = Category(comparison.head)
          val operator                      = Operator(comparison(1))
          val num                           = comparison.drop(2).toInt
          Rule(category, operator, num, target)
        }
        acc.updated(name, Workflow(rules, default))
      case (acc, _) => acc
    }

    val partsRegex = """^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}$""".r
    val parts = partsRaw.split('\n').toList.collect {
      case partsRegex(x, m, a, s) =>
        Map(Category.X -> x.toInt, Category.M -> m.toInt, Category.A -> a.toInt, Category.S -> s.toInt)
    }

    (workflows, parts)
  }

  override def part1(input: (Map[String, Workflow], List[Part])): Int = {
    val (workflows, parts) = input

    @tailrec
    def helper(part: Part, node: String = "in"): String = node match {
      case "A" | "R" => node
      case _         => helper(part, workflows(node).destination(part))
    }

    parts.foldLeft(0) {
      case (acc, part) if helper(part) == "A" => acc + part.values.sum
      case (acc, _)                           => acc
    }
  }

  override def part2(input: (Map[String, Workflow], List[Part])): Long = {
    val workflows = input._1

    def helper(
        node: String,
        spans: Map[Category, Span] = Map(
          Category.X -> Span(1, 4000),
          Category.M -> Span(1, 4000),
          Category.A -> Span(1, 4000),
          Category.S -> Span(1, 4000)
        )
    ): Long = node match {
      case "A" => spans.values.foldLeft(1L)(_ * _.size)
      case "R" => 0
      case _ =>
        val workflow = workflows(node)
        workflow.rules
          .flatMap { rule =>
            val span = spans(rule.category)
            // Filter out rules that don't apply to this span
            if (
              (rule.op == Operator.LT && span.start >= rule.value) ||
              (rule.op == Operator.GT && span.end <= rule.value)
            ) {
              None
            } else {
              Some((rule, span))
            }
          }
          .collectFirst {
            // Rule applies to all spans
            case (rule, span)
                if (rule.op == Operator.LT && span.end < rule.value) || (rule.op == Operator.GT && span.start > rule.value) =>
              helper(rule.target, spans)
            case (rule, span) if rule.op == Operator.LT =>
              val (lower, higher) = span.split(rule.value)
              helper(rule.target, spans.updated(rule.category, lower)) +
                helper(node, spans.updated(rule.category, higher))
            case (rule, span) if rule.op == Operator.GT =>
              val (lower, higher) = span.split(rule.value + 1)
              helper(node, spans.updated(rule.category, lower)) +
                helper(rule.target, spans.updated(rule.category, higher))
          }
          .getOrElse(helper(workflow.default, spans))
    }

    helper("in")
  }
}
