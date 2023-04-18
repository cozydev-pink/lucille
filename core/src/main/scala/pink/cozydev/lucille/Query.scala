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

import cats.data.NonEmptyList

sealed trait Query extends Product with Serializable {
  def mapLastTerm(f: TermQuery => Query): Query
}

sealed trait TermQuery extends Query {
  def mapLastTerm(f: TermQuery => Query): Query =
    f(this)
}

final case class MultiQuery(qs: NonEmptyList[Query]) extends Query {

  def mapLastTerm(f: TermQuery => Query): MultiQuery = {
    val newLast: Query = qs.last.mapLastTerm(f)
    if (qs.size == 1) MultiQuery(NonEmptyList.one(newLast))
    else {
      val newT = qs.tail.init :+ newLast
      MultiQuery(NonEmptyList(qs.head, newT))
    }
  }
}
object MultiQuery {
  def apply(head: Query, tail: Query*): MultiQuery =
    MultiQuery(NonEmptyList(head, tail.toList))
}

object Query {
  final case class Term(str: String) extends TermQuery
  final case class Phrase(str: String) extends TermQuery
  final case class Prefix(str: String) extends TermQuery
  final case class Proximity(str: String, num: Int) extends TermQuery
  final case class Fuzzy(str: String, num: Option[Int]) extends TermQuery
  final case class TermRegex(str: String) extends TermQuery
  final case class TermRange(
      lower: Option[String],
      upper: Option[String],
      lowerInc: Boolean,
      upperInc: Boolean,
  ) extends TermQuery

  final case class Or(qs: NonEmptyList[Query]) extends Query {
    def mapLastTerm(f: TermQuery => Query): Or =
      Or(rewriteLastTerm(qs, f))
  }
  object Or {
    def apply(head: Query, tail: Query*): Or =
      Or(NonEmptyList(head, tail.toList))
  }

  final case class And(qs: NonEmptyList[Query]) extends Query {
    def mapLastTerm(f: TermQuery => Query): And =
      And(rewriteLastTerm(qs, f))
  }
  object And {
    def apply(head: Query, tail: Query*): And =
      And(NonEmptyList(head, tail.toList))
  }

  final case class Not(q: Query) extends Query {
    def mapLastTerm(f: TermQuery => Query): Not =
      Not(q.mapLastTerm(f))
  }

  final case class Group(qs: NonEmptyList[Query]) extends Query {
    def mapLastTerm(f: TermQuery => Query): Group =
      Group(rewriteLastTerm(qs, f))
  }
  object Group {
    def apply(head: Query, tail: Query*): Group =
      Group(NonEmptyList(head, tail.toList))
  }

  final case class UnaryPlus(q: Query) extends Query {
    def mapLastTerm(f: TermQuery => Query): UnaryPlus =
      UnaryPlus(q.mapLastTerm(f))
  }
  final case class UnaryMinus(q: Query) extends Query {
    def mapLastTerm(f: TermQuery => Query): UnaryMinus =
      UnaryMinus(q.mapLastTerm(f))
  }
  final case class MinimumMatch(qs: NonEmptyList[Query], num: Int) extends Query {
    def mapLastTerm(f: TermQuery => Query): MinimumMatch =
      MinimumMatch(rewriteLastTerm(qs, f), num)
  }
  final case class Field(field: String, q: Query) extends Query {
    def mapLastTerm(f: TermQuery => Query): Field =
      Field(field, q.mapLastTerm(f))
  }

  private def rewriteLastTerm(qs: NonEmptyList[Query], f: TermQuery => Query): NonEmptyList[Query] =
    if (qs.size == 1) NonEmptyList.one(qs.head.mapLastTerm(f))
    else {
      val newT = qs.tail.init :+ qs.last.mapLastTerm(f)
      NonEmptyList(qs.head, newT)
    }
}
