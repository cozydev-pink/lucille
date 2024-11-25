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

import Query._
import munit.TestOptions

class AssociativitySuite extends munit.FunSuite {

  // Using `TestOptions` like this let's us still use `.only` on the test input/name
  def check(parser: QueryParser, prefix: String)(
      options: TestOptions,
      expected: Query,
  )(implicit loc: munit.Location): Unit = {
    val queryString = options.name
    val testName = s"$prefix '$queryString'"
    test(options.withName(testName)) {
      val actual = parser.parse(queryString)
      assertEquals(actual, Right(expected))
    }
  }

  val checkWithDefaultOr: (TestOptions, Query) => Unit =
    check(QueryParser.default, "default OR :")

  val checkWithDefaultAnd: (TestOptions, Query) => Unit =
    check(QueryParser.withDefaultOperatorAND, "default And:")

  checkWithDefaultOr(
    "NOT a AND b",
    And(Not(Term("a")), Term("b")),
  )
  checkWithDefaultAnd(
    "NOT a AND b",
    And(Not(Term("a")), Term("b")),
  )

  checkWithDefaultOr(
    "a AND NOT b",
    And(Term("a"), Not(Term("b"))),
  )
  checkWithDefaultAnd(
    "a AND NOT b",
    And(Term("a"), Not(Term("b"))),
  )

  checkWithDefaultOr(
    "a AND b OR x",
    Or(And(Term("a"), Term("b")), Term("x")),
  )
  checkWithDefaultAnd(
    "a AND b OR x",
    Or(And(Term("a"), Term("b")), Term("x")),
  )

  checkWithDefaultOr(
    "a AND b OR x AND y",
    Or(And(Term("a"), Term("b")), And(Term("x"), Term("y"))),
  )
  checkWithDefaultAnd(
    "a AND b OR x AND y",
    Or(And(Term("a"), Term("b")), And(Term("x"), Term("y"))),
  )

  checkWithDefaultOr(
    "a AND b AND c OR x",
    Or(And(Term("a"), Term("b"), Term("c")), Term("x")),
  )
  checkWithDefaultAnd(
    "a AND b AND c OR x",
    Or(And(Term("a"), Term("b"), Term("c")), Term("x")),
  )

  checkWithDefaultOr(
    "a b AND c",
    Or(Term("a"), And(Term("b"), Term("c"))),
  )
  checkWithDefaultAnd(
    "a b AND c",
    And(Term("a"), And(Term("b"), Term("c"))),
  )

  checkWithDefaultOr(
    "a b AND c d",
    Or(Term("a"), And(Term("b"), Term("c")), Term("d")),
  )
  checkWithDefaultAnd(
    "a b AND c d",
    And(Term("a"), And(Term("b"), Term("c")), Term("d")),
  )

  checkWithDefaultOr(
    "a b AND c AND d",
    Or(Term("a"), And(Term("b"), Term("c"), Term("d"))),
  )
  checkWithDefaultAnd(
    "a b AND c AND d",
    And(Term("a"), And(Term("b"), Term("c"), Term("d"))),
  )

  checkWithDefaultOr(
    "a b AND c AND d AND e",
    Or(Term("a"), And(Term("b"), Term("c"), Term("d"), Term("e"))),
  )
  checkWithDefaultAnd(
    "a b AND c AND d AND e",
    And(Term("a"), And(Term("b"), Term("c"), Term("d"), Term("e"))),
  )

  checkWithDefaultOr(
    "a b AND c AND d OR e",
    Or(Term("a"), Or(And(Term("b"), Term("c"), Term("d")), Term("e"))),
  )
  checkWithDefaultAnd(
    "a b AND c AND d OR e",
    And(Term("a"), Or(And(Term("b"), Term("c"), Term("d")), Term("e"))),
  )

  checkWithDefaultOr(
    "a b AND c OR d e",
    Or(Term("a"), Or(And(Term("b"), Term("c")), Term("d")), Term("e")),
  )
  checkWithDefaultAnd(
    "a b AND c OR d e",
    And(Term("a"), Or(And(Term("b"), Term("c")), Term("d")), Term("e")),
  )

  checkWithDefaultOr(
    "a b AND c OR d AND e",
    Or(Term("a"), Or(And(Term("b"), Term("c")), And(Term("d"), Term("e")))),
  )
  checkWithDefaultAnd(
    "a b AND c OR d AND e",
    And(Term("a"), Or(And(Term("b"), Term("c")), And(Term("d"), Term("e")))),
  )

  checkWithDefaultOr(
    "a b AND c OR d OR e",
    Or(Term("a"), Or(And(Term("b"), Term("c")), Term("d"), Term("e"))),
  )
  checkWithDefaultAnd(
    "a b AND c OR d OR e",
    And(Term("a"), Or(And(Term("b"), Term("c")), Term("d"), Term("e"))),
  )

}
