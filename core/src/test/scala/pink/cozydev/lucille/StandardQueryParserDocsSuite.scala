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
import Query._
import Parser._

// Cases taken from the Lucene StandardQueryParser docs
// https://lucene.apache.org/core/9_4_1/queryparser/org/apache/lucene/queryparser/flexible/standard/StandardQueryParser.html
class StandardQueryParserDocsSuite extends munit.FunSuite {

  test("test") {
    val r = parseQ("test")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(Term("test"))
      ),
    )
  }

  test("test equipment") {
    val r = parseQ("test equipment")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(Term("test"), Term("equipment"))
      ),
    )
  }

  test("\"test failure\"~4") {
    val r = parseQ("\"test failure\"~4")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(Proximity("test failure", 4))
      ),
    )
  }

  test("tes*") {
    val r = parseQ("tes*")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(Prefix("tes"))
      ),
    )
  }

  test("/.est(s|ing)/") {
    val r = parseQ("/.est(s|ing)/")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(TermRegex(".est(s|ing)"))
      ),
    )
  }

  test("nest~4") {
    val r = parseQ("nest~4")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(Fuzzy("nest", Some(4)))
      ),
    )
  }

  test("title:test") {
    val r = parseQ("title:test")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(Field("title", Term("test")))
      ),
    )
  }

  test("title:(die OR hard)") {
    val r = parseQ("title:(die OR hard)")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          Field("title", Group(Or(Term("die"), Term("hard"))))
        )
      ),
    )
  }

  test("test AND results") {
    val r = parseQ("test AND results")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(And(Term("test"), Term("results")))
      ),
    )
  }

  test("title:test AND NOT title:complete") {
    val r = parseQ("title:test AND NOT title:complete")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          And(
            Field("title", Term("test")),
            Not(Field("title", Term("complete"))),
          )
        )
      ),
    )
  }

  test("title:test AND (pass* OR fail*)") {
    val r = parseQ("title:test AND (pass* OR fail*)")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          And(
            Field("title", Term("test")),
            Group(
              Or(
                Prefix("pass"),
                Prefix("fail"),
              )
            ),
          )
        )
      ),
    )
  }

  test("title:(pass fail skip)") {
    val r = parseQ("title:(pass fail skip)")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          Field("title", Group(Term("pass"), Term("fail"), Term("skip")))
        )
      ),
    )
  }

  test("title:(+test +\"result unknown\")") {
    val r = parseQ("title:(+test +\"result unknown\")")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          Field(
            "title",
            Group(
              UnaryPlus(Term("test")),
              UnaryPlus(Phrase("result unknown")),
            ),
          )
        )
      ),
    )
  }

  test("name:[Jones TO Smith]") {
    val r = parseQ("name:[Jones TO Smith]")
    assertEquals(
      r,
      Right(NonEmptyList.of(Field("name", TermRange(Some("Jones"), Some("Smith"), true, true)))),
    )
  }

  test("score:{2.5 TO 7.3}") {
    val r = parseQ("score:{2.5 TO 7.3}")
    assertEquals(
      r,
      Right(NonEmptyList.of(Field("score", TermRange(Some("2.5"), Some("7.3"), false, false)))),
    )
  }

  test("score:{2.5 TO *]") {
    val r = parseQ("score:{2.5 TO *]")
    assertEquals(
      r,
      Right(NonEmptyList.of(Field("score", TermRange(Some("2.5"), None, false, true)))),
    )
  }

  test("jones^2 OR smith^0.5".fail) {
    val r = parseQ("jones^2 OR smith^0.5")
    assert(r.isRight)
  }

  test("field:(a OR b NOT c)^2.5 OR field:d".fail) {
    val r = parseQ("field:(a OR b NOT c)^2.5 OR field:d")
    assert(r.isRight)
  }

  test("""\:\(quoted\+term\)\:""".fail) {
    val r = parseQ("""\:\(quoted\+term\)\:""")
    assert(r.isRight)
  }

  test("(blue crab fish)@2") {
    val r = parseQ("(blue crab fish)@2")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          MinimumMatch(NonEmptyList.of(Term("blue"), Term("crab"), Term("fish")), 2)
        )
      ),
    )
  }

  test("((yellow OR blue) crab fish)@2") {
    val r = parseQ("((yellow OR blue) crab fish)@2")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          MinimumMatch(
            NonEmptyList.of(
              Group(
                Or(Term("yellow"), Term("blue"))
              ),
              Term("crab"),
              Term("fish"),
            ),
            2,
          )
        )
      ),
    )
  }

  // TODO parses but not into "interval function clauses"
  test("fn:ordered(quick brown fox)") {
    val r = parseQ("fn:ordered(quick brown fox)")
    assert(r.isRight)
  }

  // TODO parses but not into "interval function clauses"
  test("title:fn:maxwidth(5 fn:atLeast(2 quick brown fox))") {
    val r = parseQ("title:fn:maxwidth(5 fn:atLeast(2 quick brown fox))")
    assert(r.isRight)
  }

}
