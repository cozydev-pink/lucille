/*
 * Copyright 2022 CozyDev
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

package pink.cozydev.lucille

import cats.parse.{Parser => P}
import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.Parser.{char => pchar}
import cats.data.NonEmptyList
import cats.parse.Parser0
import cats.syntax.all._

object Parser {
  import Query._

  val dquote = pchar('"')
  val spaces: P[Unit] = P.charIn(Set(' ', '\t')).rep.void
  val maybeSpace: Parser0[Unit] = spaces.?.void
  val int: P[Int] = (digit.rep <* P.not(P.char('.'))).string.map(_.toInt)

  private val alwaysSpecial = Set('\\', ':', '^', '(', ')', '"', ' ', '*', '~')
  private val allowed: P[Char] =
    // From cats.parse.strings.Json nonEscaped handling
    P.charIn((0x20.toChar to 0x10ffff.toChar).toSet -- alwaysSpecial)
  val reserved = Set("OR", "||", "AND", "&&", "NOT", "+", "-", "/")

  // Term query
  // e.g. 'cat', 'catch22'
  val term: P[String] = P.not(P.stringIn(reserved)).with1 *> allowed.rep.string
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
  val fuzzyT: P[FuzzyTerm] = (fuzzySoft ~ int.?).map { case (q, n) =>
    FuzzyTerm(q, n)
  }

  // Prefix term
  // e.g. 'jump*'
  val prefixT: P[PrefixTerm] =
    (term.soft <* P.char('*'))
      .map(PrefixTerm.apply)

  val or = (P.string("OR") | P.string("||")).as(Op.OR)
  val and = (P.string("AND") | P.string("&&")).as(Op.AND)
  val infixOp = or | and

  // given "  OR term1 OR   term2$"
  // parses completely
  // given "  OR term1 OR   term2 extra$"
  // parses until the end of 'term2', with 'extra' being left
  def suffixOps(query: P[Query]): Parser0[List[(Op, Query)]] =
    ((maybeSpace.with1 *> infixOp <* sp.rep) ~ query)
      .repUntil0(maybeSpace.with1 *> query)

  // parse simple queries followed by suffix op queries
  // "q0 q1 OR q2"
  def qWithSuffixOps(query: P[Query]): P[NonEmptyList[Query]] =
    (query.repSep(sp.rep) ~ suffixOps(query))
      .map { case (h, t) => Op.associateOps(h, t) }

  // parse a whole list of queries
  // "q0 q1 OR q2 q3"
  // we repeat so that we can parse q3
  // the first iteration only gets "qp q1 OR q2"
  def nonGrouped(query: P[Query]): P[NonEmptyList[Query]] =
    (maybeSpace.with1 *> qWithSuffixOps(query)).rep.map(_.flatten)

  // Not query
  // e.g. 'animals NOT (cats AND dogs)'
  def notQ(query: P[Query]): P[Query] =
    ((P.string("NOT").soft ~ maybeSpace) *> query).map(NotQ.apply)

  // Group query
  // e.g. '(cats AND dogs)'
  def groupQ(query: P[Query]): P[Group] = {
    val g = nonGrouped(query)
      .between(P.char('('), P.char(')'))
    val endOfGroupSpecial = P.char('@')
    (g <* P.not(endOfGroupSpecial)).map(Group.apply)
  }

  // Field query
  // e.g. 'title:cats', 'author:"Silly Goose"', 'title:(cats AND dogs)'
  val fieldValueSoft: P[String] = term.soft <* pchar(':')
  def fieldQuery(query: P[Query]): P[FieldQ] =
    (fieldValueSoft ~ query).map { case (f, q) => FieldQ(f, q) }

  // Unary Plus query
  // e.g. '+cat', '+(cats AND dogs)'
  def unaryPlus(query: P[Query]): P[UnaryPlus] =
    P.char('+') *> query.map(UnaryPlus.apply)

  // Unary Minus query
  // e.g. 'cat', '-(cats AND dogs)'
  def unaryMinus(query: P[Query]): P[UnaryMinus] =
    P.char('-') *> query.map(UnaryMinus.apply)

  // Range query
  // e.g. '{cats TO dogs}', '[1 TO *}'
  def rangeQuery: P[RangeQ] = {
    val inclLower = P.charIn('{', '[').map(lowerBound => lowerBound == '[') <* maybeSpace
    val inclUpper = maybeSpace *> P.charIn('}', ']').map(upperBound => upperBound == ']')
    val boundValue =
      P.char('*').as(None) | P.not(P.stringIn(reserved)).with1 *> (alpha | digit | P.char(
        '.'
      )).rep.string
        .map(Some(_))
    val to = spaces *> P.string("TO") <* spaces
    (inclLower ~ boundValue ~ to ~ boundValue ~ inclUpper)
      .map { case ((((il, l), _), u), iu) =>
        RangeQ(l, u, il, iu)
      }
  }

  // Tie compound queries together recursively
  // Order is very important here
  // prefixT before termQ
  val recursiveQ: P[Query] = P.recursive[Query](r =>
    P.oneOf(
      List(
        unaryPlus(r),
        unaryMinus(r),
        notQ(r),
        fieldQuery(r),
        proximityQ,
        rangeQuery,
        fuzzyT,
        prefixT,
        termQ,
        phraseQ,
        groupQ(r),
      )
    )
  )

  // One or more queries implicitly grouped together in a list
  val fullQuery = nonGrouped(recursiveQ)

  // TODO we need to deal with the trailing whitespace now that we support groups
  def parseQ(s: String) = fullQuery.parseAll(s.stripTrailing)
}
