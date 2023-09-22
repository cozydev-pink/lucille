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

/** A trait for all queries */
sealed trait Query extends Product with Serializable {

  /** Builds a new query by applying a `Term => Query` function to a Term if it is in the last position.
    *
    * @param f the function to apply to the last TermQuery
    * @return
    */
  def mapLastTerm(f: Query.Term => Query): Query
}

/** A trait for all leaf node queries (meaning that they do not contain queries) */
sealed trait TermQuery extends Query {
  // noop for everything except Query.Term
  def mapLastTerm(f: Query.Term => Query): Query = this
}

/** A trait for a list of one or more queries */
final case class MultiQuery(qs: NonEmptyList[Query]) extends Query {

  def mapLastTerm(f: Query.Term => Query): MultiQuery = {
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

  /** A term query
    * e.g. 'cat', 'catch22'
    */
  final case class Term(str: String) extends TermQuery {
    override def mapLastTerm(f: Query.Term => Query): Query =
      f(this)
  }

  /** A phrase query
    * e.g. 'the cat jumped'
    */
  final case class Phrase(str: String) extends TermQuery

  /** A prefix query
    * Search for words starting with the given prefix
    * e.g. 'jump*'
    */
  final case class Prefix(str: String) extends TermQuery

  /** A proximity query
    * Search for words within a specified word distance
    * e.g. '"cat jumped"~3', '"one two three"~2'
    */
  final case class Proximity(str: String, num: Int) extends TermQuery

  /** A fuzzy query
    * Search for words within a Damerau-Levenshtein Distance
    * The additional parameter, between 0 and 2, specifies the number of edits allowed (defaults to 2)
    * e.g. 'cat~', 'cat~1'
    */
  final case class Fuzzy(str: String, num: Option[Int]) extends TermQuery

  /** A regex query
    * Search with a regular expression, the pattern is given between forward slashes, `/`.
    * e.g. '/.ump(s|ing)'
    */
  final case class TermRegex(str: String) extends TermQuery

  /** A range query
    * Search for terms that fall between some upper and lower bounds. The bounds can be inclusive or exclusive.
    * e.g. '{cats TO dogs}', '[1 TO *]'
    */
  final case class TermRange(
      lower: Option[String],
      upper: Option[String],
      lowerInc: Boolean,
      upperInc: Boolean,
  ) extends TermQuery

  /** An Or operator
    * Join the given queries with OR, the equivalent of taking the union of the results of each query
    * e.g. 'q1 OR q2'
    */
  final case class Or(qs: NonEmptyList[Query]) extends Query {
    def mapLastTerm(f: Query.Term => Query): Or =
      Or(rewriteLastTerm(qs, f))
  }
  object Or {
    def apply(head: Query, tail: Query*): Or =
      Or(NonEmptyList(head, tail.toList))
  }

  /**  An And operator
    * Join the given queries with AND, the equivalent of taking the intersection of the results of each query
    * e.g. 'q1 AND q2'
    */
  final case class And(qs: NonEmptyList[Query]) extends Query {
    def mapLastTerm(f: Query.Term => Query): And =
      And(rewriteLastTerm(qs, f))
  }
  object And {
    def apply(head: Query, tail: Query*): And =
      And(NonEmptyList(head, tail.toList))
  }

  /** A Not operator
    * Exclude terms that would match the given query
    * e.g. 'NOT cats'
    */
  final case class Not(q: Query) extends Query {
    def mapLastTerm(f: Query.Term => Query): Not =
      Not(q.mapLastTerm(f))
  }

  /** A group query
    * Queries grouped together with parentheses
    * e.g. '(cats AND dogs)'
    */
  final case class Group(qs: NonEmptyList[Query]) extends Query {
    def mapLastTerm(f: Query.Term => Query): Group = this
  }
  object Group {
    def apply(head: Query, tail: Query*): Group =
      Group(NonEmptyList(head, tail.toList))
  }

  /** A unary plus query
    * Search for documents which must contain the given query
    * e.g. '+cat', '+(cats AND dogs)'
    */
  final case class UnaryPlus(q: Query) extends Query {
    def mapLastTerm(f: Query.Term => Query): UnaryPlus =
      UnaryPlus(q.mapLastTerm(f))
  }

  /** A unary minus query
    * Search for documents which must not contain the given query
    * e.g. '-cat', '-(cats AND dogs)'
    */
  final case class UnaryMinus(q: Query) extends Query {
    def mapLastTerm(f: Query.Term => Query): UnaryMinus =
      UnaryMinus(q.mapLastTerm(f))
  }

  /** A minimum match query
    * Search for documents that match at least `num` of the given queries
    * e.g. '(one two three)@2'
    */
  final case class MinimumMatch(qs: NonEmptyList[Query], num: Int) extends Query {
    def mapLastTerm(f: Query.Term => Query): MinimumMatch = this
  }

  /** A field query
    * Search for documents by applying the given query only to the named field
    * e.g. 'author:"Silly Goose"', 'title:(cats AND dogs)'
    */
  final case class Field(field: String, q: Query) extends Query {
    def mapLastTerm(f: Query.Term => Query): Field =
      Field(field, q.mapLastTerm(f))
  }

  private def rewriteLastTerm(
      qs: NonEmptyList[Query],
      f: Query.Term => Query,
  ): NonEmptyList[Query] =
    if (qs.size == 1) NonEmptyList.one(qs.head.mapLastTerm(f))
    else {
      val newT = qs.tail.init :+ qs.last.mapLastTerm(f)
      NonEmptyList(qs.head, newT)
    }
}
