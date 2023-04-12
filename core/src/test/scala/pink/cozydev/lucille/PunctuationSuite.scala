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
import cats.parse.Parser.Error
import Query._
import Parser._

// Similar to the SingleSimpleQuerySuite but with a focus on queries with punctuation
class PunctuationSuite extends munit.FunSuite {

  def assertSingleQ(r: Either[Error, MultiQuery], expected: Query)(implicit
      loc: munit.Location
  ) =
    assertEquals(r, Right(MultiQuery(NonEmptyList.one(expected))))

  test("parse single term with period") {
    val r = parseQ("typelevel.com")
    assertSingleQ(r, Term("typelevel.com"))
  }

  test("parse single term with slash") {
    val r = parseQ("typelevel.com/cats")
    assertSingleQ(r, Term("typelevel.com/cats"))
  }

  test("parse single term with dash") {
    val r = parseQ("cats-effect")
    assertSingleQ(r, Term("cats-effect"))
  }

  test("parse single term with '@'") {
    val r = parseQ("first.last@email.com")
    assertSingleQ(r, Term("first.last@email.com"))
  }

  test("parse fieldQ with phraseQ with dash") {
    val r = parseQ("name:\"cats-effect\"")
    assertSingleQ(r, Field("name", Phrase("cats-effect")))
  }

}
