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

class QuerySuite extends munit.FunSuite {
  def expandQ(q: Query): Query =
    q match {
      case Query.Term(t) => Query.Or(Query.Term(t), Query.Prefix(t))
      case _ => q
    }

  test("MultiQuery.mapLastTerm maps last Term in last Query (OR)") {
    val mq = MultiQuery(Or(Term("cats"), Term("dogs")))
    val expected = MultiQuery(Or(Term("cats"), Or(Term("dogs"), Prefix("dogs"))))
    assertEquals(mq.mapLastTerm(expandQ), expected)
  }

  test("MultiQuery.mapLastTerm maps last Term in last Query (AND)") {
    val mq = MultiQuery(And(Term("cats"), Term("dogs")))
    val expected = MultiQuery(And(Term("cats"), Or(Term("dogs"), Prefix("dogs"))))
    assertEquals(mq.mapLastTerm(expandQ), expected)
  }

  test("MultiQuery.mapLastTerm maps last Term in last Query (NOT)") {
    val mq = MultiQuery(Term("cats"), Not(Term("dogs")))
    val expected = MultiQuery(Term("cats"), Not(Or(Term("dogs"), Prefix("dogs"))))
    assertEquals(mq.mapLastTerm(expandQ), expected)
  }

  test("MultiQuery.mapLastTerm maps last Term in last Query (OR + NOT)") {
    val mq = MultiQuery(Or(Term("cats"), Not(Term("dogs"))))
    val expected = MultiQuery(Or(Term("cats"), Not(Or(Term("dogs"), Prefix("dogs")))))
    assertEquals(mq.mapLastTerm(expandQ), expected)
  }

  test("MultiQuery.mapLastTerm does nothing for ending minimum-should-match query") {
    val qs = "(apple banana orange)@2"
    val mq = Parser.parseQ(qs)
    assertEquals(mq.map(_.mapLastTerm(expandQ)), mq)
  }

  test("MultiQuery.mapLastTerm does nothing for ending range query") {
    val qs = "name:[cats TO fs2]"
    val mq = Parser.parseQ(qs)
    assertEquals(mq.map(_.mapLastTerm(expandQ)), mq)
  }

  test("MultiQuery.mapLastTerm does nothing for ending group query") {
    val qs = "cats AND (dogs OR fish)"
    val mq = Parser.parseQ(qs)
    assertEquals(mq.map(_.mapLastTerm(expandQ)), mq)
  }

  test("Query.and performs logical AND with Query and argument") {
    val q1 = Term("cats")
    val q2 = Or(NonEmptyList.of(Term("dogs"), Term("fish")))
    val expected = And(NonEmptyList.of(q1, q2))
    assertEquals(q1.and(q2), expected)
  }

  test("Query.or performs logical OR with Query and argument") {
    val q1 = Term("dogs")
    val q2 = Term("cats")
    val expected = Or(NonEmptyList.of(q1, q2))
    assertEquals(q1.or(q2), expected)
  }

  test("Query.not negates Query") {
    val q1 = Term("cats")
    val expected = Not(q1)
    assertEquals(q1.not, expected)
  }

  test("Query.boost boosts Query with argument") {
    val q1 = Term("dogs")
    val expected = Boost(q1, 100)
    assertEquals(q1.boost(100), expected)
  }
}
