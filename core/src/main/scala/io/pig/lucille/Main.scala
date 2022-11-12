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

object Parser {

  sealed trait Query extends Product with Serializable
  final case class SimpleQ(q: String) extends Query
  final case class FieldQ(field: String, q: String) extends Query
  final case class ProximityQ(q: String, num: Int) extends Query
  final case class FuzzyTerm(q: String, num: Option[Int]) extends Query
  final case class OrQ(left: Query, right: Query) extends Query

  val dquote = pchar('"')
  val term: P[String] = alpha.rep.string
  val phrase: P[String] = (term ~ sp.?).rep.surroundedBy(dquote).string
  val termClause: P[String] = term | phrase
  val searchWords: P[SimpleQ] =
    (phrase | (term ~ sp.?).rep.string).map(SimpleQ.apply)

  val fieldName: P[String] = alpha.rep.string
  val fieldValueSoft: P[String] = fieldName.soft <* pchar(':')
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
    P.oneOf(fieldQuery :: proximityQuery :: fuzzyTerm :: searchWords :: Nil)

  val or = P.string("OR") | P.string("||")
  def orQ(pa: P[Query]): P[Query] = ((pa.soft <* or) ~ pa).map { case (l, r) => OrQ(l, r) }

  val pq: P[Query] = P.recursive[Query] { recurse =>
    P.oneOf(simpleQ :: orQ(recurse) :: Nil)
  }

  val query = pq.repSep0(sp)

}
