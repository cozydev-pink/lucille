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

object Query {

  /** A term query
    * e.g. 'cat', 'catch22'
    *
    *  @param str the term
    */
  final case class Term(str: String) extends TermQuery {
    override def mapLastTerm(f: Query.Term => Query): Query =
      f(this)
  }

  /** A phrase query
    * e.g. 'the cat jumped'
    *
    * @param str the phrase
    */
  final case class Phrase(str: String) extends TermQuery

  /** A prefix query
    * Search for words starting with the given prefix
    * e.g. 'jump*'
    *
    * @param str the prefix
    */
  final case class Prefix(str: String) extends TermQuery

  /** A proximity query
    * Search for words within a specified word distance
    * e.g. '"cat jumped"\~3', '"one two three"\~2'
    *
    * @param str the words
    * @param num the word distance
    */
  final case class Proximity(str: String, num: Int) extends TermQuery

  /** A fuzzy query with an optional distance value
    * e.g. 'cat\~', 'cat\~1'
    *
    * @param str the string
    * @param num the number of edits allowed
    */
  final case class Fuzzy(str: String, num: Option[Int]) extends TermQuery

  /** A regex query
    * Search with a regular expression, the pattern is given between forward slashes, `/`.
    * e.g. '/.ump(s|ing)'
    *
    * @param str the regular expression query
    */
  final case class TermRegex(str: String) extends TermQuery

  /** A range query
    * Search for terms that fall between some upper and lower bounds. The bounds can be inclusive or exclusive.
    * e.g. '{cats TO dogs}', '[1 TO *]'
    *
    * @param lower the lower bound
    * @param upper the upper bound
    * @param lowerInc whether the lower bound is inclusive
    * @param upperInc whether the upper bound is inclusive
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
    *
    * @param qs the queries to union
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
    *
    * @param qs the queries to intersect
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
    *
    * @param q the query to exclude
    */
  final case class Not(q: Query) extends Query {
    def mapLastTerm(f: Query.Term => Query): Not =
      Not(q.mapLastTerm(f))
  }

  /** A group query
    * Queries grouped together with parentheses
    * e.g. '(cats AND dogs)'
    *
    * @param qs the queries to group
    */
  final case class Group(q: Query) extends Query {
    def mapLastTerm(f: Query.Term => Query): Group = this
  }

  /** A unary plus query
    * Search for documents which must contain the given query
    * e.g. '+cat', '+(cats AND dogs)'
    *
    * @param q the query
    */
  final case class UnaryPlus(q: Query) extends Query {
    def mapLastTerm(f: Query.Term => Query): UnaryPlus =
      UnaryPlus(q.mapLastTerm(f))
  }

  /** A unary minus query
    * Search for documents which must not contain the given query
    * e.g. '-cat', '-(cats AND dogs)'
    *
    * @param q the query
    */
  final case class UnaryMinus(q: Query) extends Query {
    def mapLastTerm(f: Query.Term => Query): UnaryMinus =
      UnaryMinus(q.mapLastTerm(f))
  }

  /** A query with a boost weight
    * Search for documents with the underlying query as usual, the boost is only used in scoring
    * e.g. 'cats^2 OR dogs^3.1'
    *
    * @param q the query
    * @param boost the boost weight
    */
  final case class Boost(q: Query, boost: Float) extends Query {
    def mapLastTerm(f: Query.Term => Query): Boost = this
  }

  /** A minimum match query
    * Search for documents that match at least `num` of the given queries
    * e.g. '(one two three)@2'
    *
    * @param qs the queries
    * @param num the number of queries that must match
    */
  final case class MinimumMatch(qs: NonEmptyList[Query], num: Int) extends Query {
    def mapLastTerm(f: Query.Term => Query): MinimumMatch = this
  }

  /** A field query
    * Search for documents by applying the given query only to the named field
    * e.g. 'author:"Silly Goose"', 'title:(cats AND dogs)'
    *
    * @param field the field name
    * @param q the query
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
