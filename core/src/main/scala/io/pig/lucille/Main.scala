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

import cats.parse.{Parser => P, Parser0}
import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.Parser.{char => pchar}

object Parser {

  sealed trait Query extends Product with Serializable
  final case class SimpleQ(q: String) extends Query
  final case class FieldQ(field: String, q: String) extends Query
  final case class ProximityQ(q: String, num: Int) extends Query

  val dquote = pchar('"')
  val term = alpha.rep.string
  val phrase = (term ~ sp.?).rep.surroundedBy(dquote).string
  val termClause = term | phrase
  val searchWords: Parser0[SimpleQ] =
    (phrase | (term ~ sp.?).rep.string).map(SimpleQ.apply)

  val fieldName = alpha.rep.string
  val fieldValueSoft = fieldName.soft <* pchar(':')
  val fieldQuery: Parser0[FieldQ] =
    (fieldValueSoft ~ termClause).map { case (f, q) => FieldQ(f, q) }

  val proxD = digit.rep.string.map(_.toInt)
  val proxSoft = phrase.soft <* pchar('^')
  val proximityQuery: Parser0[ProximityQ] = (proxSoft ~ proxD).map { case (p, n) =>
    ProximityQ(p, n)
  }

  val pq: Parser0[Query] = P.oneOf0(fieldQuery :: proximityQuery :: searchWords :: Nil)

  // From https://github.com/typelevel/cats-parse/issues/205
  private def rep0sep0[A](
      data: Parser0[A],
      separator: P[Any],
  ): Parser0[List[A]] =
    (data.? ~ (separator *> data).rep0).map { case (a, as) => a ++: as }

  val query = rep0sep0(pq, sp)

}
