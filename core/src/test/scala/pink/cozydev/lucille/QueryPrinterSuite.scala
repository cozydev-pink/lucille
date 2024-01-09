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

class QueryPrinterSimpleQueriesSuite extends munit.FunSuite {

  test("prints MultiQuery query") {
    val q = MultiQuery(NonEmptyList.of(Term("hello"), Term("hi")))
    val str = QueryPrinter.print(q)
    assertEquals(str, "hello hi")
  }

  test("prints OR query") {
    val q = Or(NonEmptyList.of(Term("hello"), Term("hi")))
    val str = QueryPrinter.print(q)
    assertEquals(str, "hello OR hi")
  }

  test("prints AND query") {
    val q = And(NonEmptyList.of(Term("hello"), Term("hi")))
    val str = QueryPrinter.print(q)
    assertEquals(str, "hello AND hi")
  }

  test("prints Not query") {
    val q = Not(Group(NonEmptyList.of(Term("hello"), Term("hi"))))
    val str = QueryPrinter.print(q)
    assertEquals(str, "NOT (hello hi)")
  }

  test("prints Group query") {
    val q = Group(NonEmptyList.of(Term("hello"), Term("hi")))
    val str = QueryPrinter.print(q)
    assertEquals(str, "(hello hi)")
  }

  test("prints UnaryMinus query") {
    val q = UnaryMinus(Term("hello"))
    val str = QueryPrinter.print(q)
    assertEquals(str, "-hello")
  }

  test("prints UnaryPlus query") {
    val q = UnaryPlus(Term("hello"))
    val str = QueryPrinter.print(q)
    assertEquals(str, "+hello")
  }

  test("prints MinimumMatch query") {
    val q = MinimumMatch(NonEmptyList.of(Term("hello"), Term("hi")), 2)
    val str = QueryPrinter.print(q)
    assertEquals(str, "(hello hi)@2")
  }

  test("prints Field query") {
    val q = Field("msg", MinimumMatch(NonEmptyList.of(Term("hello"), Term("hi")), 2))
    val str = QueryPrinter.print(q)
    assertEquals(str, "msg:(hello hi)@2")
  }

}

class QueryPrinterSimpleQueryTermSuite extends munit.FunSuite {

  test("prints single term") {
    val q = Term("hello")
    val str = QueryPrinter.print(q)
    assertEquals(str, "hello")
  }

  test("prints phrase") {
    val q = Phrase("hello friend")
    val str = QueryPrinter.print(q)
    assertEquals(str, "\"hello friend\"")
  }

  test("prints prefix term") {
    val q = Prefix("hel")
    val str = QueryPrinter.print(q)
    assertEquals(str, "hel*")
  }

  test("prints proximity term") {
    val q = Proximity("cats jumped", 2)
    val str = QueryPrinter.print(q)
    assertEquals(str, "\"cats jumped\"~2")
  }

  test("prints fuzzy (no num) term") {
    val q = Fuzzy("hello", None)
    val str = QueryPrinter.print(q)
    assertEquals(str, "hello~")
  }

  test("prints fuzzy (num) term") {
    val q = Fuzzy("hello", Some(2))
    val str = QueryPrinter.print(q)
    assertEquals(str, "hello~2")
  }

  test("prints regex term") {
    val q = TermRegex("/.ump(s|ing)/")
    val str = QueryPrinter.print(q)
    assertEquals(str, "/.ump(s|ing)/")
  }

  test("prints term range [* TO *]") {
    val q = TermRange(None, None, false, false)
    val str = QueryPrinter.print(q)
    assertEquals(str, "[* TO *]")
  }

  test("prints term range [Apple TO Banana]") {
    val q = TermRange(Some("Apple"), Some("Banana"), false, false)
    val str = QueryPrinter.print(q)
    assertEquals(str, "[Apple TO Banana]")
  }

  test("prints term range {Apple TO Banana]") {
    val q = TermRange(Some("Apple"), Some("Banana"), true, false)
    val str = QueryPrinter.print(q)
    assertEquals(str, "{Apple TO Banana]")
  }

  test("prints term range [Apple TO Banana}") {
    val q = TermRange(Some("Apple"), Some("Banana"), false, true)
    val str = QueryPrinter.print(q)
    assertEquals(str, "[Apple TO Banana}")
  }

}
