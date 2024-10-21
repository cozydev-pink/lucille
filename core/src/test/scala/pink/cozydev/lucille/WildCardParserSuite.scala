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
import WildCardOp._
import cats.data.NonEmptyList

class WildCardParserSuite extends munit.FunSuite {

  // We already have a `Prefix` query
  // do we make this a special case of the larger `Wildcard` query?
  val parseQ = QueryParser.withDefaultOperatorOR.parse(_)

  test("trailing single char") {
    val r = parseQ("cat?")
    assertEquals(r, Right(WildCard(NonEmptyList.of(Str("cat"), SingleChar))))
  }

  test("trailing many char") {
    val r = parseQ("cat*")
    assertEquals(r, Right(Prefix("cat")))
  }

  test("leading single char") {
    val r = parseQ("?cat")
    assertEquals(r, Right(WildCard(NonEmptyList.of(SingleChar, Str("cat")))))
  }

  test("leading many char") {
    val r = parseQ("*cat")
    assertEquals(r, Right(WildCard(NonEmptyList.of(ManyChar, Str("cat")))))
  }

  test("middle many char") {
    val r = parseQ("cat*tail")
    assertEquals(r, Right(WildCard(NonEmptyList.of(Str("cat"), ManyChar, Str("tail")))))
  }

  test("middle single char") {
    val r = parseQ("cat?tail")
    assertEquals(r, Right(WildCard(NonEmptyList.of(Str("cat"), SingleChar, Str("tail")))))
  }

  test("trailing single char grouped") {
    val r = parseQ("(cat?)")
    assertEquals(r, Right(Group(WildCard(NonEmptyList.of(Str("cat"), SingleChar)))))
  }

  test("trailing many char grouped") {
    val r = parseQ("(cat*)")
    assertEquals(r, Right(Group(Prefix("cat"))))
  }

  test("leading single char grouped") {
    val r = parseQ("(?cat)")
    assertEquals(r, Right(Group(WildCard(NonEmptyList.of(SingleChar, Str("cat"))))))
  }

  test("leading many char grouped") {
    val r = parseQ("(*cat)")
    assertEquals(r, Right(Group(WildCard(NonEmptyList.of(ManyChar, Str("cat"))))))
  }

  test("middle many char grouped") {
    val r = parseQ("(cat*tail)")
    assertEquals(r, Right(Group(WildCard(NonEmptyList.of(Str("cat"), ManyChar, Str("tail"))))))
  }

  test("middle single char grouped") {
    val r = parseQ("(cat?tail)")
    assertEquals(r, Right(Group(WildCard(NonEmptyList.of(Str("cat"), SingleChar, Str("tail"))))))
  }

  test("trailing single char boosted") {
    val r = parseQ("cat?^2")
    assertEquals(r, Right(Boost(WildCard(NonEmptyList.of(Str("cat"), SingleChar)), 2.0f)))
  }

  test("trailing many char boosted") {
    val r = parseQ("cat*^2")
    assertEquals(r, Right(Boost(Prefix("cat"), 2.0f)))
  }

  test("leading single char boosted") {
    val r = parseQ("?cat^2")
    assertEquals(r, Right(Boost(WildCard(NonEmptyList.of(SingleChar, Str("cat"))), 2.0f)))
  }

  test("leading many char boosted") {
    val r = parseQ("*cat^2")
    assertEquals(r, Right(Boost(WildCard(NonEmptyList.of(ManyChar, Str("cat"))), 2.0f)))
  }

  test("middle many char boosted") {
    val r = parseQ("cat*tail^2")
    assertEquals(
      r,
      Right(Boost(WildCard(NonEmptyList.of(Str("cat"), ManyChar, Str("tail"))), 2.0f)),
    )
  }

  test("middle single char boosted") {
    val r = parseQ("cat?tail^2")
    assertEquals(
      r,
      Right(Boost(WildCard(NonEmptyList.of(Str("cat"), SingleChar, Str("tail"))), 2.0f)),
    )
  }

}
