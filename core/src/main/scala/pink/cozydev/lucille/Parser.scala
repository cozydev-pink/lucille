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

  val dquote = P.charIn(Set('"', '“', '”'))
  val spaces: P[Unit] = P.charIn(Set(' ', '\t')).rep.void
  val maybeSpace: Parser0[Unit] = spaces.?.void
  val int: P[Int] = (digit.rep <* P.not(P.char('.'))).string.map(_.toInt)

  private val baseRange = (0x20.toChar to 0x10ffff.toChar).toSet
  private val special = Set('\\', ':', '^', '(', ')', '"', '“', '”', ' ', '*', '~')
  private val allowed: P[Char] =
    // From cats.parse.strings.Json nonEscaped handling
    P.charIn(baseRange -- special)
  val reserved = Set("OR", "||", "AND", "&&", "NOT", "+", "-", "/")

  val term: P[String] = P.not(P.stringIn(reserved)).with1 *> allowed.rep.string

  /** Parse a term query
    * e.g. 'cat', 'catch22'
    */
  val termQ: P[Term] = term.map(Term.apply)

  val phrase: P[String] = (term ~ sp.?).rep.string.surroundedBy(dquote)

  /** Parse a phrase query
    * e.g. 'the cat jumped'
    */
  val phraseQ: P[Phrase] = phrase.map(Phrase.apply)

  val proxSoft: P[String] = phrase.soft <* pchar('~')

  /** Parse a proximity query
    * e.g. '"cat jumped"~3', '"one two three"~2'
    */
  val proximityQ: P[Proximity] = (proxSoft ~ int).map { case (p, n) =>
    Proximity(p, n)
  }

  val fuzzySoft: P[String] = term.soft <* pchar('~')

  /** Parse a fuzzy term query
    * e.g. 'cat~', 'cat~t'
    */
  val fuzzyT: P[Fuzzy] = (fuzzySoft ~ int.?).map { case (q, n) =>
    Fuzzy(q, n)
  }

  /** Parse a prefix term query
    * e.g. 'jump*'
    */
  val prefixT: P[Prefix] =
    (term.soft <* P.char('*'))
      .map(Prefix.apply)

  private val regex: P[String] = {
    val notEscape = P.charIn(baseRange - '\\' - '/').void
    val rStr = notEscape.orElse(P.char('\\') *> P.char('/')).rep.string
    rStr.surroundedBy(pchar('/'))
  }

  /** Parse a regex query
    * e.g. '/.ump(s|ing)'
    */
  val regexQ: P[TermRegex] = regex.map(TermRegex.apply)

  val or = (P.string("OR") | P.string("||")).as(Op.OR)
  val and = (P.string("AND") | P.string("&&")).as(Op.AND)
  val infixOp = (or | and).withContext("infixOp")

  /** Parse a suffix op query
    * e.g. 'OR term1 OR term2$' parses completely
    * however 'OR term1 OR term2 extra$' parses until the end of 'term2', with 'extra' being left
    */
  def suffixOps(query: P[Query]): Parser0[List[(Op, Query)]] =
    ((maybeSpace.with1 *> infixOp <* sp.rep) ~ query)
      .repUntil0(maybeSpace *> (P.end | query))

  /** Parse simple queries followed by suffix op queries
    * e.g. 'q0 q1 OR q2'
    */
  def qWithSuffixOps(query: P[Query]): P[NonEmptyList[Query]] =
    (query.repSep(sp.rep) ~ suffixOps(query))
      .map { case (h, t) => Op.associateOps(h, t) }

  /** Parse a whole list of queries
    * e.g. 'q0 q1 OR q2 q3'
    * Parsing repeats so that we can parse q3, the first iteration only gets 'q0 q1 OR q2'
    */
  def nonGrouped(query: P[Query]): P[NonEmptyList[Query]] =
    (maybeSpace.with1 *> qWithSuffixOps(query)).repUntil(maybeSpace ~ P.end).map(_.flatten)

  /** Parse a not query
    * e.g. 'animals NOT (cats AND dogs)'
    */
  def notQ(query: P[Query]): P[Query] =
    ((P.string("NOT").soft ~ maybeSpace) *> query).map(Not.apply)

  /**  Parse a minimum match query
    * e.g. '(one two three)@2'
    */
  def minimumMatchQ(query: P[Query]): P[MinimumMatch] = {
    val matchNum = P.char('@') *> int
    val grouped = nonGrouped(query).between(P.char('('), P.char(')'))
    (grouped.soft ~ matchNum).map { case (qs, n) => MinimumMatch(qs, n) }
  }

  /**  Parse a group query
    * e.g. '(cats AND dogs)'
    */
  def groupQ(query: P[Query]): P[Group] = {
    val g = nonGrouped(query)
      .between(P.char('('), P.char(')'))
    val endOfGroupSpecial = P.char('@')
    (g <* P.not(endOfGroupSpecial)).map(Group.apply)
  }

  /** Parse a field query
    * e.g. 'title:cats', 'author:"Silly Goose"', 'title:(cats AND dogs)'
    */
  val fieldValueSoft: P[String] = term.soft <* pchar(':')
  def fieldQuery(query: P[Query]): P[Field] =
    (fieldValueSoft ~ query).map { case (f, q) => Field(f, q) }

  /** Parse a unary plus query
    * e.g. '+cat', '+(cats AND dogs)'
    */
  def unaryPlus(query: P[Query]): P[UnaryPlus] =
    P.char('+') *> query.map(UnaryPlus.apply)

  /** Parse a unary minus query
    * e.g. 'cat'*, '-(cats AND dogs)'
    */
  def unaryMinus(query: P[Query]): P[UnaryMinus] =
    P.char('-') *> query.map(UnaryMinus.apply)

  /** Parse a term range query
    * e.g. '{cats TO dogs}, '[1 TO *}''
    */
  def rangeQuery: P[TermRange] = {
    val inclLower = P.charIn('{', '[').map(lowerBound => lowerBound == '[') <* maybeSpace
    val inclUpper = maybeSpace *> P.charIn('}', ']').map(upperBound => upperBound == ']')
    val wild = P.char('*').as(None)
    val txt = (alpha.void | digit.void | P.char('.')).rep.string.map(Some(_))
    val boundValue = wild | P.not(P.stringIn(reserved)).with1 *> txt
    val to = spaces *> P.string("TO") <* spaces
    (inclLower ~ boundValue ~ to ~ boundValue ~ inclUpper)
      .map { case ((((il, l), _), u), iu) =>
        TermRange(l, u, il, iu)
      }
  }

  /** Recursively parse compound queries
    * The order is very important:
    * - prefixT must come before termQ
    */
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
        regexQ,
        phraseQ,
        minimumMatchQ(r),
        groupQ(r),
      )
    )
  )

  /** Parse one or more queries implicitly grouped together in a list
    */
  val fullQuery = nonGrouped(recursiveQ) <* maybeSpace

  /** Attempt to parse a whole string representing a Lucene query
    */
  def parseQ(s: String): Either[cats.parse.Parser.Error, MultiQuery] =
    fullQuery.parseAll(s).map(MultiQuery.apply)
}
