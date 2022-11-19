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
import cats.syntax.all._

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
  final case class Group(qs: NonEmptyList[Query]) extends Query

  val dquote = pchar('"')
  val spaces: P[Unit] = P.charIn(Set(' ', '\t')).rep.void
  val maybeSpace: Parser0[Unit] = spaces.?.void
  val int: P[Int] = digit.rep.string.map(_.toInt)

  // Term query
  // e.g. 'cat', 'catch22'
  val reserved = Set("OR", "||", "AND", "&&", "NOT")
  val term: P[String] = P.not(P.stringIn(reserved)).with1 *> (alpha | digit).rep.string
  val termQ: P[TermQ] = term.map(TermQ.apply)

  // Phrase query
  // e.g. 'the cat jumped'
  val phrase: P[String] = (term ~ sp.?).rep.string.surroundedBy(dquote)
  val phraseQ: P[PhraseQ] = phrase.map(PhraseQ.apply)

  // Proximity query
  // e.g. '"cat jumped"~3', '"one two three"~2'
  val proxSoft: P[String] = phrase.soft <* pchar('~')
  val proximityQ: P[ProximityQ] = (proxSoft ~ int).map { case (p, n) =>
    ProximityQ(p, n)
  }

  // Fuzzy term
  // e.g. 'cat~', 'cat~2'
  val fuzzySoft: P[String] = term.soft <* pchar('~')
  val fuzzyTerm: P[FuzzyTerm] = (fuzzySoft ~ int.?).map { case (q, n) =>
    FuzzyTerm(q, n)
  }

  sealed trait Op extends Product with Serializable
  case object OR extends Op
  case object AND extends Op

  val or = (P.string("OR") | P.string("||")).as(OR)
  val and = (P.string("AND") | P.string("&&")).as(AND)
  val infixOp = or | and

  /** @param q1 queries parsed so far, the last one could be part of a suffixOp
    * @param qs suffixOp and query pairs
    * @return
    */
  def associateOps(q1: NonEmptyList[Query], opQs: List[(Op, Query)]): NonEmptyList[Query] = {
    def go(acc: NonEmptyList[Query], op: Op, opQs: List[(Op, Query)]): NonEmptyList[Query] =
      opQs match {
        case Nil =>
          op match {
            // no more ops to pair
            case OR => NonEmptyList.of(OrQ(acc))
            case AND => NonEmptyList.of(AndQ(acc))
          }
        case (nextOp, q) :: tailOpP =>
          (op, nextOp) match {
            case (OR, OR) => go(acc.append(q), nextOp, tailOpP)
            case (AND, AND) => go(acc.append(q), nextOp, tailOpP)
            case (AND, OR) =>
              go(NonEmptyList.of(q), nextOp, tailOpP).prepend(AndQ(acc))
            case (OR, AND) =>
              // TODO we only get away with not wrapping the `allButLast` in an OrQ
              //  because `OR` is the default query type. This should be configurable
              val allButLast = NonEmptyList(acc.head, acc.tail.dropRight(1))
              allButLast.concatNel(go(NonEmptyList.of(acc.last, q), nextOp, tailOpP))
          }
      }

    opQs match {
      case Nil => q1
      case opHead :: _ =>
        q1 match {
          case NonEmptyList(_, Nil) => go(q1, opHead._1, opQs)
          case NonEmptyList(h, atLeastOneQ) =>
            // multiple queries on the left, we'll look at just the last one
            val allButLast = NonEmptyList(h, atLeastOneQ.dropRight(1))
            allButLast.concatNel(go(NonEmptyList.of(q1.last), opHead._1, opQs))
        }
    }
  }

  // given "  OR term1 OR   term2$"
  // parses completely
  // given "  OR term1 OR   term2 extra$"
  // parses until the end of 'term2', with 'extra' being left
  def suffixOps(query: P[Query]): Parser0[List[(Op, Query)]] =
    ((maybeSpace.with1 *> infixOp <* sp.rep) ~ query)
      .repUntil0(maybeSpace.with1 *> query)

  // val not = P.string("NOT")
  // def notQ(pa: P[Query]): P[Query] = (not *> pa).map(NotQ.apply)

  // parse simple queries followed by suffix op queries
  // "q0 q1 OR q2"
  def qWithSuffixOps(query: P[Query]): P[NonEmptyList[Query]] =
    (query.repSep(sp.rep) ~ suffixOps(query))
      .map { case (h, t) => associateOps(h, t) }

  // parse a whole list of queries
  // "q0 q1 OR q2 q3"
  // we repeat so that we can parse q3
  // the first iteration only gets "qp q1 OR q2"
  def nonGrouped(query: P[Query]): P[NonEmptyList[Query]] =
    (maybeSpace.with1 *> qWithSuffixOps(query)).rep.map(_.flatten)

  // Group query
  // e.g. '(cats AND dogs)'
  def groupQ(query: P[Query]): P[Group] =
    nonGrouped(query)
      .between(P.char('('), P.char(')'))
      .map(Group.apply)

  // Field query
  // e.g. 'title:cats', 'author:"Silly Goose"', 'title:(cats AND dogs)'
  val fieldValueSoft: P[String] = term.soft <* pchar(':')
  def fieldQuery(query: P[Query]): P[FieldQ] =
    (fieldValueSoft ~ query).map { case (f, q) => FieldQ(f, q) }

  // Tie compound queries together recursively
  // Order is very important here
  val recursiveQ: P[Query] =
    P.recursive[Query](r =>
      P.oneOf(
        fieldQuery(r) :: proximityQ :: fuzzyTerm :: termQ :: phraseQ :: groupQ(r) :: Nil
      )
    )

  // One or more queries implicitly grouped together in a list
  val fullQuery = nonGrouped(recursiveQ)

  // TODO we need to deal with the trailing whitespace now that we support groups
  def parseQ(s: String) = fullQuery.parseAll(s.stripTrailing)
}
