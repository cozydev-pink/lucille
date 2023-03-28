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

class RegexSuite extends munit.FunSuite {

  def assertSingleQ(r: Either[Error, NonEmptyList[Query]], expected: Query)(implicit
      loc: munit.Location
  ) =
    assertEquals(r, Right(NonEmptyList.one(expected)))

  test("parse single regex with wildcard star") {
    val r = parseQ("/jump.*/")
    assertSingleQ(r, Regex("jump.*"))
  }

  test("does not parse without ending slash") {
    val r = parseQ("/jump.*")
    assert(r.isLeft)
  }

  test("parse regex with repeat min-max") {
    val r = parseQ("/hi{1,5}/")
    assertSingleQ(r, Regex("hi{1,5}"))
  }

  test("parse multipe regex in a group") {
    val r = parseQ("(/jump.*/ /.ouse/)")
    assertSingleQ(r, Group(NonEmptyList.of(Regex("jump.*"), Regex(".ouse"))))
  }

  test("fails to parse regex with escaped slash".fail) {
    val r = parseQ("""/home\/.*/""")
    assertSingleQ(r, Regex("""home\/.*"""))
  }

}
