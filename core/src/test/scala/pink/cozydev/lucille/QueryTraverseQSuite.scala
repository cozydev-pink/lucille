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
import cats.data.NonEmptyList

class QueryTraverseQSuite extends munit.FunSuite {
  def upperCaseTerms(q: Query): Either[String, Query] = q match {
    case Term(str) => Right(Term(str.toUpperCase))
    case q => Right(q)
  }
  def onlyCats(q: Query): Either[String, Query] = q match {
    case Term("cats") => Right(Term("CATS"))
    case _ => Left("oops")
  }
  def termToPhrase(q: Query): Either[String, Query] = q match {
    case Term(t) => Right(Phrase(t))
    case q => Right(q)
  }

  test("Query.And/Or.traverseQ maps inner queries and preserves structure") {
    val qAnd = And(Term("cats"), Term("dogs"))
    val qOr = Or(Term("cats"), Term("dogs"))
    val actualAnd = qAnd.traverseQ(upperCaseTerms)
    val actualOr = qOr.traverseQ(upperCaseTerms)
    assertEquals(actualAnd, Right(And(Term("CATS"), Term("DOGS"))))
    assertEquals(actualOr, Right(Or(Term("CATS"), Term("DOGS"))))
  }

  test("Query.And/Or.traverseQ short circuits") {
    val qAnd = And(Term("cats"), Term("dogs"))
    val qOr = Or(Term("cats"), Term("dogs"))
    val actualAnd = qAnd.traverseQ(onlyCats)
    val actualOr = qOr.traverseQ(onlyCats)
    assertEquals(actualAnd, Left("oops"))
    assertEquals(actualOr, Left("oops"))
  }

  test("Query.traverseQ short circuits when big tree has one `Left` node") {
    val cat = Term("cats")
    val q = And(
      cat,
      Boost(cat, 2.2f),
      Field("title", cat),
      Or(
        Not(cat),
        UnaryPlus(cat),
        UnaryMinus(cat),
        Group(And(cat, cat)),
        MinimumMatch(NonEmptyList.of(cat, And(cat, Term("dogs"))), 2),
      ),
    )
    val actual = q.traverseQ(onlyCats)
    assertEquals(actual, Left("oops"))
  }

  test("Query.traverseQ maps all nodes in big tree") {
    val cat = Term("cats")
    val q = And(
      cat,
      Boost(cat, 2.2f),
      Field("title", cat),
      Or(
        Not(cat),
        UnaryPlus(cat),
        UnaryMinus(cat),
        Group(And(cat, cat)),
        MinimumMatch(NonEmptyList.of(cat, And(cat, Term("dogs"))), 2),
      ),
    )
    val actual = q.traverseQ(upperCaseTerms)
    val bigCat = Term("CATS")
    val expected = And(
      bigCat,
      Boost(bigCat, 2.2f),
      Field("title", bigCat),
      Or(
        Not(bigCat),
        UnaryPlus(bigCat),
        UnaryMinus(bigCat),
        Group(And(bigCat, bigCat)),
        MinimumMatch(NonEmptyList.of(bigCat, And(bigCat, Term("DOGS"))), 2),
      ),
    )
    assertEquals(actual, Right(expected))
  }

  test("Query.And.traverseQ allows changing query types") {
    val q = And(Term("cats"), Not(Term("dogs")))
    val actualAnd = q.traverseQ(termToPhrase)
    assertEquals(actualAnd, Right(And(Phrase("cats"), Not(Phrase("dogs")))))
  }
}
