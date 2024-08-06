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

class DefaultBooleanAndSuite extends munit.FunSuite {

  val parser = QueryParser.withDefaultOperatorAND
  val parseQ = parser.parse(_)

  test("DefaultBooleanAnd two terms") {
    val actual = parseQ("cats dogs")
    val expected = And(Term("cats"), Term("dogs"))
    assertEquals(actual, Right(expected))
  }

  test("DefaultBooleanAnd many terms") {
    val actual = parseQ("cats dogs fish lizards")
    val expected = And(Term("cats"), Term("dogs"), Term("fish"), Term("lizards"))
    assertEquals(actual, Right(expected))
  }

  test("DefaultBooleanAnd terms in a group") {
    val actual = parseQ("(cats dogs)")
    val expected = Group(And(Term("cats"), Term("dogs")))
    assertEquals(actual, Right(expected))
  }

  test("DefaultBooleanAnd terms in a group with explicit AND") {
    val actual = parseQ("(cats AND dogs)")
    val expected = Group(And(Term("cats"), Term("dogs")))
    assertEquals(actual, Right(expected))
  }

  test("DefaultBooleanAnd terms in a group with explicit OR") {
    val actual = parseQ("(cats OR dogs)")
    val expected = Group(Or(Term("cats"), Term("dogs")))
    assertEquals(actual, Right(expected))
  }

}
