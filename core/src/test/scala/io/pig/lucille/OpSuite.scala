/*
 * Copyright 2022 Cozydev.pink
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
import Op._

class AssociateOpsSuite extends munit.FunSuite {

  test("associates ORs") {
    val leftQs = NonEmptyList.of(TermQ("the"), TermQ("cat"))
    val opQs = List((OR, TermQ("dog")))
    val result = associateOps(leftQs, opQs)
    val expected = NonEmptyList.of(TermQ("the"), OrQ(NonEmptyList.of(TermQ("cat"), TermQ("dog"))))
    assertEquals(result, expected)
  }

  test("associates ANDs") {
    val leftQs = NonEmptyList.of(TermQ("the"), TermQ("cat"))
    val opQs = List((AND, TermQ("dog")))
    val result = associateOps(leftQs, opQs)
    val expected = NonEmptyList.of(TermQ("the"), AndQ(NonEmptyList.of(TermQ("cat"), TermQ("dog"))))
    assertEquals(result, expected)
  }

  test("associates multiple ORs") {
    val leftQs = NonEmptyList.of(TermQ("the"), TermQ("cat"))
    val opQs = List((OR, TermQ("dog")), (OR, TermQ("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(TermQ("the"), OrQ(NonEmptyList.of(TermQ("cat"), TermQ("dog"), TermQ("fish"))))
    assertEquals(result, expected)
  }

  test("associates multiple ANDs") {
    val leftQs = NonEmptyList.of(TermQ("the"), TermQ("cat"))
    val opQs = List((AND, TermQ("dog")), (AND, TermQ("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        TermQ("the"),
        AndQ(NonEmptyList.of(TermQ("cat"), TermQ("dog"), TermQ("fish"))),
      )
    assertEquals(result, expected)
  }

  test("associates with OR and then AND") {
    // the cat OR ocean AND fish
    // default:the default:cat +default:ocean +default:fish
    val leftQs = NonEmptyList.of(TermQ("the"), TermQ("cat"))
    val opQs = List((OR, TermQ("ocean")), (AND, TermQ("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        TermQ("the"),
        TermQ("cat"),
        AndQ(NonEmptyList.of(TermQ("ocean"), TermQ("fish"))),
      )
    assertEquals(result, expected)
  }

  test("associates with AND and then OR") {
    // the cat AND ocean OR fish
    // default:the +default:cat +default:ocean default:fish
    val leftQs = NonEmptyList.of(TermQ("the"), TermQ("cat"))
    val opQs = List((AND, TermQ("ocean")), (OR, TermQ("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        TermQ("the"),
        AndQ(NonEmptyList.of(TermQ("cat"), TermQ("ocean"))),
        OrQ(NonEmptyList.of(TermQ("fish"))),
      )
    assertEquals(result, expected)
  }

  test("associates with two ORs and then AND") {
    // the cat OR ocean OR ocean2 AND fish
    // default:the default:cat default:ocean +default:ocean2 +default:fish
    val leftQs = NonEmptyList.of(TermQ("the"), TermQ("cat"))
    val opQs = List((OR, TermQ("ocean")), (OR, TermQ("ocean2")), (AND, TermQ("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        TermQ("the"),
        TermQ("cat"),
        TermQ("ocean"),
        AndQ(NonEmptyList.of(TermQ("ocean2"), TermQ("fish"))),
      )
    assertEquals(result, expected)
  }

  test("associates with two ANDs and then OR") {
    // the cat AND ocean AND ocean2 OR fish
    // default:the +default:cat +default:ocean +default:ocean2 default:fish
    val leftQs = NonEmptyList.of(TermQ("the"), TermQ("cat"))
    val opQs = List((AND, TermQ("ocean")), (AND, TermQ("ocean2")), (OR, TermQ("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        TermQ("the"),
        AndQ(NonEmptyList.of(TermQ("cat"), TermQ("ocean"), TermQ("ocean2"))),
        OrQ(NonEmptyList.of(TermQ("fish"))),
      )
    assertEquals(result, expected)
  }
}
