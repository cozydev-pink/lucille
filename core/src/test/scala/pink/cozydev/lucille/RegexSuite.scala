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

class RegexSuite extends munit.FunSuite {

  val parseQ = QueryParser.parse(_)

  test("parse single regex with wildcard star") {
    val r = parseQ("/jump.*/")
    assertEquals(r, Right(TermRegex("jump.*")))
  }

  test("does not parse without ending slash") {
    val r = parseQ("/jump.*")
    assert(r.isLeft)
  }

  test("parse regex with repeat min-max") {
    val r = parseQ("/hi{1,5}/")
    assertEquals(r, Right(TermRegex("hi{1,5}")))
  }

  test("parse multipe regex in a group") {
    val r = parseQ("(/jump.*/ /.ouse/)")
    assertEquals(r, Right(Group(Or(TermRegex("jump.*"), TermRegex(".ouse")))))
  }

  test("parse regex with escaped slash") {
    val r = parseQ("""/home\/.*/""")
    assertEquals(r, Right(TermRegex("""home\/.*""")))
  }

}
