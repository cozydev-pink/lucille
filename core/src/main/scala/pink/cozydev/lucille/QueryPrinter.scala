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

import pink.cozydev.lucille.Query._
import pink.cozydev.lucille.QueryParserHelpers._
import cats.data.NonEmptyList

object QueryPrinter {

  /** Builds a string representation of a `Query`.
    * `Boost` query values are rounded to `precision`.
    *
    * @example {{{
    * scala> QueryPrinter.print(Boost(Phrase("apple pi"), 3.14159f), 3)
    * res0: String = "apple pi"^3.142
    * }}}
    *
    * @param query query to format
    * @param precision restricts the number of characters after the decimal
    * @return
    */
  def print(query: Query, precision: Int = 2): String = {
    val sb = new StringBuilder()

    def printQ(query: Query): Unit =
      query match {
        case q: TermQuery => strTermQuery(q)
        case q: Or => printEachNel(q.qs, " OR ")
        case q: And => printEachNel(q.qs, " AND ")
        case q: Not =>
          sb.append("NOT ")
          printQ(q.q)
        case q: Group =>
          sb.append('(')
          printQ(q.q)
          sb.append(')')
        case q: UnaryPlus =>
          sb.append('+')
          printQ(q.q)
        case q: UnaryMinus =>
          sb.append('-')
          printQ(q.q)
        case q: MinimumMatch =>
          sb.append('(')
          printEachNel(q.qs, " ")
          sb.append(s")@${q.num}")
        case q: Boost =>
          printBoostQuery(q)
        case q: Field =>
          sb.append(q.field)
          sb.append(':')
          printQ(q.q)
      }

    def printBoostQuery(q: Boost): Unit = {
      val boostStr = s"%.${precision}f".format(q.boost)
      q.q match {
        case q: Group => printQ(q)
        case q: Phrase => strTermQuery(q)
        case q: Term => strTermQuery(q)
        case qq =>
          sb.append('(')
          printQ(qq)
          sb.append(')')
      }
      sb.append('^')
      sb.append(boostStr)
    }

    def strTermQuery(q: TermQuery): Unit =
      q match {
        case q: Term => escapeStr(q.str)
        case q: Phrase =>
          sb.append('"')
          escapePhrase(q.str)
          sb.append('"')
        case q: Prefix =>
          sb.append(q.str)
          sb.append('*')
        case q: Proximity =>
          sb.append('"')
          sb.append(q.str)
          sb.append("\"~")
          sb.append(q.num.toString())
        case q: Fuzzy =>
          sb.append(q.str)
          sb.append('~')
          q.num.foreach(i => sb.append(i.toString()))
        case q: TermRegex => sb.append(q.str)
        case q: TermRange =>
          if (q.lowerInc) sb.append('{') else sb.append('[')
          sb.append(q.lower.getOrElse("*"))
          sb.append(" TO ")
          sb.append(q.upper.getOrElse("*"))
          if (q.upperInc) sb.append('}') else sb.append(']')
        case q: WildCard =>
          printWildCard(q)
      }

    def printWildCard(q: WildCard): Unit = {
      def printOp(op: WildCardOp): Unit =
        op match {
          case WildCardOp.SingleChar => sb.append('?')
          case WildCardOp.ManyChar => sb.append('*')
          case WildCardOp.Str(str) => sb.append(str)
        }
      q.ops.iterator.foreach(printOp)
    }

    def printEachNel(nel: NonEmptyList[Query], sep: String): Unit = {
      printQ(nel.head)
      nel.tail.foreach { q =>
        sb.append(sep)
        printQ(q)
      }
    }

    def escapeStr(str: String): Unit =
      str.foreach { c =>
        if (luceneSpecial.contains(c)) sb.append('\\')
        sb.append(c)
      }

    def escapePhrase(str: String): Unit =
      str.foreach { c =>
        if (escapedTokensPhraseSet.contains(c)) sb.append('\\')
        sb.append(c)
      }

    printQ(query)
    sb.result()
  }
}
