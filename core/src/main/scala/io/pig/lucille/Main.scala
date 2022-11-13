/*
 * Copyright 2022 Pig.io
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.pig.lucille

import cats.parse.{Parser => P}
import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.Parser.{char => pchar}
import cats.data.NonEmptyList
import cats.parse.Parser0

object Parser {

  sealed trait Query extends Product with Serializable
  final case class TermQ(q: String) extends Query
  final case class PhraseQ(q: String) extends Query
  final case class FieldQ(field: String, q: Query) extends Query
  final case class ProximityQ(q: String, num: Int) extends Query
  final case class FuzzyTerm(q: String, num: Option[Int]) extends Query
  final case class OrQ(qs: NonEmptyList[Query]) extends Query
  final case class AndQ(qs: NonEmptyList[Query]) extends Query
  final case class NotQ(q: Query) extends Query

  val dquote = pchar('"')
  val spaces: P[Unit] = P.charIn(Set(' ', '\t')).rep.void
  val maybeSpace: Parser0[Unit] = spaces.?.void

  // Trying to fail on 'OR' quickly
  val reserved = Set("OR", "||", "AND", "&&", "NOT")
  val term: P[String] = P.not(P.stringIn(reserved)).with1 *> alpha.rep.string
  val termQ: P[TermQ] = term.map(TermQ.apply)

  val phrase: P[String] = (term ~ sp.?).rep.string.surroundedBy(dquote)
  val phraseQ: P[PhraseQ] = phrase.map(PhraseQ.apply)
  val termClause: P[Query] = termQ | phraseQ

  val fieldName: P[String] = alpha.rep.string
  val fieldValueSoft: P[String] = fieldName.soft <* pchar(':')
  // Should this be a query?
  val fieldQuery: P[FieldQ] =
    (fieldValueSoft ~ termClause).map { case (f, q) => FieldQ(f, q) }

  val int: P[Int] = digit.rep.string.map(_.toInt)
  // TODO can this be a full phrase or only a 2 word phrase?
  val proxSoft: P[String] = phrase.soft <* pchar('~')
  val proximityQuery: P[ProximityQ] = (proxSoft ~ int).map { case (p, n) =>
    ProximityQ(p, n)
  }

  val fuzzySoft: P[String] = term.soft <* pchar('~')
  val fuzzyTerm: P[FuzzyTerm] = (fuzzySoft ~ int.?).map { case (q, n) =>
    FuzzyTerm(q, n)
  }

  val simpleQ: P[Query] =
    P.oneOf(fieldQuery :: proximityQuery :: fuzzyTerm :: termQ :: phraseQ :: Nil)

  sealed trait Op extends Product with Serializable
  case object OR extends Op
  case object AND extends Op

  val or = (P.string("OR") | P.string("||")).as(OR)
  val and = (P.string("AND") | P.string("&&")).as(AND)
  val infixOp = or | and
  def associateOps(q1: NonEmptyList[Query], qs: List[(Op, Query)]): NonEmptyList[Query] =
    qs match {
      case Nil => q1
      case head :: _ => // TODO recurse
        head match {
          case (OR, q) => NonEmptyList.of(OrQ(NonEmptyList.of(q1.last, q)))
          case (AND, q) => NonEmptyList.of(AndQ(NonEmptyList.of(q1.last, q)))
        }
    }

  // "  OR term OR   term$"
  def opSuffix(pa: P[Query]): Parser0[List[(Op, Query)]] =
    ((maybeSpace.with1 *> infixOp <* sp.rep) ~ pa).rep0

  // val not = P.string("NOT")
  // def notQ(pa: P[Query]): P[Query] = (not *> pa).map(NotQ.apply)

  def qWithSuffixOps(pa: P[Query]) = (pa.repSep(sp.rep) ~ opSuffix(pa)).map { case (h, t) =>
    associateOps(h, t)
  }

  val query: P[NonEmptyList[Query]] = qWithSuffixOps(simpleQ)

  def parseQ(s: String) = query.parseAll(s.trim.replaceAll(" +", " "))

}
