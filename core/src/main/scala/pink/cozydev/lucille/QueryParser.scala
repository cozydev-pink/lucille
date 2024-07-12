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
import cats.parse.Rfc5234.{sp, alpha, digit, wsp}
import cats.parse.Parser.{char => pchar}
import cats.data.NonEmptyList
import cats.parse.Parser0
import cats.syntax.all._
import internal.Op

class QueryParser(
    defaultBooleanOR: Boolean
) {
  import Query._
  import Parser._

  /** Parse a not query
    * e.g. 'animals NOT (cats AND dogs)'
    */
  private def notQ(query: P[Query]): P[Query] =
    ((P.string("NOT").soft ~ maybeSpace) *> query).map(Not.apply)

  /** Parse a unary plus query
    * e.g. '+cat', '+(cats AND dogs)'
    */
  private def unaryPlus(query: P[Query]): P[UnaryPlus] =
    P.char('+') *> query.map(UnaryPlus.apply)

  /** Parse a unary minus query
    * e.g. 'cat'*, '-(cats AND dogs)'
    */
  private def unaryMinus(query: P[Query]): P[UnaryMinus] =
    P.char('-') *> query.map(UnaryMinus.apply)

  /** Parse a field query
    * e.g. 'title:cats', 'author:"Silly Goose"', 'title:(cats AND dogs)'
    */
  private val fieldValueSoft: P[String] = term.soft <* pchar(':')
  private def fieldQuery(query: P[Query]): P[Field] =
    (fieldValueSoft ~ query).map { case (f, q) => Field(f, q) }

  /**  Parse a boost query
    * e.g. 'cats^2', '(dogs)^3.1', 'field:term^2.5'
    */
  private def boostQ(query: P[Query]): P[Boost] = {
    val limitedQ = fieldQuery(query) | termQ | phraseQ | groupQ(query)
    (limitedQ.withContext("limitedQ").soft ~ (P.char('^') *> float <* queryEnd)).map(qf =>
      Boost(qf._1, qf._2)
    )
  }

  /**  Parse a minimum match query
    * e.g. '(one two three)@2'
    */
  private def minimumMatchQ(query: P[Query]): P[MinimumMatch] = {
    val matchNum = P.char('@') *> int <* queryEnd
    val grouped = nelQueries(query).between(P.char('('), P.char(')'))
    (grouped.soft ~ matchNum).map { case (qs, n) => MinimumMatch(qs, n) }
  }

  /** Parse a whole list of queries without wrapping in the default boolean operator.
    * e.g. 'q0 q1 OR q2 q3'.
    * Parsing repeats so that we can parse q3, the first iteration only gets 'q0 q1 OR q2'
    */
  private def nelQueries(query: P[Query]): P[NonEmptyList[Query]] =
    (maybeSpace.with1 *> qWithSuffixOps(query)).repUntil(maybeSpace ~ P.end).map(_.flatten)

  /** Parse simple queries followed by suffix op queries
    * e.g. 'q0 q1 OR q2'
    */
  private def qWithSuffixOps(query: P[Query]): P[NonEmptyList[Query]] =
    (query.repSep(sp.rep) ~ suffixOps(query))
      .map { case (h, t) => internal.Op.associateOps(h, t, defaultBooleanOR) }

  /** Parse a suffix op query
    * e.g. 'OR term1 OR term2$' parses completely
    * however 'OR term1 OR term2 extra$' parses until the end of 'term2', with 'extra' being left
    */
  private def suffixOps(query: P[Query]): Parser0[List[(Op, Query)]] =
    ((maybeSpace.with1 *> infixOp <* sp.rep) ~ query)
      .repUntil0(maybeSpace *> (P.end | query))

  /**  Parse a group query
    * e.g. '(cats AND dogs)'
    */
  private def groupQ(query: P[Query]): P[Group] = {
    val g = wrappedQueries(query)
      .between(P.char('('), P.char(')'))
    val endOfGroupSpecial = P.char('@')
    (g <* P.not(endOfGroupSpecial)).map(Group.apply)
  }

  /** Parse a whole list of queries and wrap them in the default boolean operator.
    * e.g. 'q0 q1 OR q2 q3'.
    * Parsing repeats so that we can parse q3, the first iteration only gets 'q0 q1 OR q2'
    */
  private def wrappedQueries(query: P[Query]): P[Query] =
    nelQueries(query).map {
      case NonEmptyList(singleQ, Nil) => singleQ
      case multipleQs =>
        if (defaultBooleanOR) Or.apply(multipleQs) else And.apply(multipleQs)
    }

  /** Recursively parse compound queries
    * The order is very important:
    * - prefixT must come before termQ
    */
  private val recursiveQ: P[Query] = P.recursive[Query](r =>
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
        minimumMatchQ(r),
        boostQ(r),
        termQ,
        regexQ,
        phraseQ,
        groupQ(r),
      )
    )
  )

  /** Parse one or more queries implicitly grouped together in a list */
  val fullQuery = wrappedQueries(recursiveQ) <* maybeSpace

  private def errorMsg(err: cats.parse.Parser.Error): String = {
    val exps = err.expected.map(_.show).mkString_("\n")
    s"Parse error at offset ${err.failedAtOffset}, with expectations:\n $exps"
  }

  /** Attempt to parse a whole string representing a Lucene query */
  def parse(input: String): Either[String, Query] =
    fullQuery
      .parseAll(input)
      .leftMap(errorMsg)

}
object QueryParser {

  val defaultParser = new QueryParser(defaultBooleanOR = true)

  /** Attempt to parse a whole string representing a Lucene query */
  def parse(input: String): Either[String, MultiQuery] =
    defaultParser.parse(input).map(q => MultiQuery(q))

}

private object Parser {
  import Query._

  val dquote = P.charIn(Set('"', '“', '”'))
  val spaces: P[Unit] = wsp.rep.void
  val maybeSpace: Parser0[Unit] = spaces.?.void
  val int: P[Int] = (digit.rep <* P.not(P.char('.'))).string.map(_.toInt)

  def parseFloat(s: String): Option[Float] =
    try Option(java.lang.Float.parseFloat(s))
    catch {
      case _: NumberFormatException => None
    }

  val float: P[Float] = {
    val dotDigits = P.char('.') *> digit.rep
    val fs = (digit.rep ~ dotDigits.?).string
    fs.mapFilter(parseFloat).withContext("float")
  }

  private val baseRange = (0x20.toChar to 0x10ffff.toChar).toSet
  private val special = Set('\\', ':', '^', '(', ')', '"', '“', '”', ' ', '*', '~')
  private val allowed: P[Char] =
    // From cats.parse.strings.Json nonEscaped handling
    P.charIn(baseRange -- special)
  val reserved = Set("OR", "||", "AND", "&&", "NOT", "+", "-", "/")

  val queryEnd = (wsp | P.end | P.char(')')).peek

  val term: P[String] = P.not(P.stringIn(reserved)).with1 *> allowed.rep.string

  /** Parse a term query
    * e.g. 'cat', 'catch22'
    */
  val termQ: P[Term] = term.map(Term.apply)

  val phrase: P[String] = (maybeSpace.with1 *> term <* maybeSpace).rep.string.surroundedBy(dquote)

  /** Parse a phrase query
    * e.g. 'the cat jumped'
    */
  val phraseQ: P[Phrase] = phrase.map(Phrase.apply)

  val proxSoft: P[String] = phrase.soft <* pchar('~')

  /** Parse a proximity query
    * e.g. '"cat jumped"\~3', '"one two three"\~2'
    */
  val proximityQ: P[Proximity] = (proxSoft ~ int).map { case (p, n) =>
    Proximity(p, n)
  }

  val fuzzySoft: P[String] = term.soft <* pchar('~')

  /** Parse a fuzzy term query
    * e.g. 'cat\~', 'cat\~1'
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

  val or = (P.string("OR") | P.string("||")).as(internal.Op.OR)
  val and = (P.string("AND") | P.string("&&")).as(internal.Op.AND)
  val infixOp = (or | and).withContext("infixOp")
}
