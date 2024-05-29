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

class QuerySuite extends munit.FunSuite {

  val parseQ = QueryParser.parse

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
    val mq = parseQ(qs)
    assertEquals(mq.map(_.mapLastTerm(expandQ)), mq)
  }

  test("MultiQuery.mapLastTerm does nothing for ending range query") {
    val qs = "name:[cats TO fs2]"
    val mq = parseQ(qs)
    assertEquals(mq.map(_.mapLastTerm(expandQ)), mq)
  }

  test("MultiQuery.mapLastTerm does nothing for ending group query") {
    val qs = "cats AND (dogs OR fish)"
    val mq = parseQ(qs)
    assertEquals(mq.map(_.mapLastTerm(expandQ)), mq)
  }
}
