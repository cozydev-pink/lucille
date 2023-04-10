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
import Op._

class AssociateOpsSuite extends munit.FunSuite {

  test("associates ORs") {
    val leftQs = NonEmptyList.of(Term("the"), Term("cat"))
    val opQs = List((OR, Term("dog")))
    val result = associateOps(leftQs, opQs)
    val expected = NonEmptyList.of(Term("the"), Or(Term("cat"), Term("dog")))
    assertEquals(result, expected)
  }

  test("associates ANDs") {
    val leftQs = NonEmptyList.of(Term("the"), Term("cat"))
    val opQs = List((AND, Term("dog")))
    val result = associateOps(leftQs, opQs)
    val expected = NonEmptyList.of(Term("the"), And(Term("cat"), Term("dog")))
    assertEquals(result, expected)
  }

  test("associates multiple ORs") {
    val leftQs = NonEmptyList.of(Term("the"), Term("cat"))
    val opQs = List((OR, Term("dog")), (OR, Term("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(Term("the"), Or(Term("cat"), Term("dog"), Term("fish")))
    assertEquals(result, expected)
  }

  test("associates multiple ANDs") {
    val leftQs = NonEmptyList.of(Term("the"), Term("cat"))
    val opQs = List((AND, Term("dog")), (AND, Term("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        Term("the"),
        And(Term("cat"), Term("dog"), Term("fish")),
      )
    assertEquals(result, expected)
  }

  test("associates with OR and then AND") {
    // the cat OR ocean AND fish
    // default:the default:cat +default:ocean +default:fish
    val leftQs = NonEmptyList.of(Term("the"), Term("cat"))
    val opQs = List((OR, Term("ocean")), (AND, Term("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        Term("the"),
        Term("cat"),
        And(Term("ocean"), Term("fish")),
      )
    assertEquals(result, expected)
  }

  test("associates with AND and then OR") {
    // the cat AND ocean OR fish
    // default:the +default:cat +default:ocean default:fish
    val leftQs = NonEmptyList.of(Term("the"), Term("cat"))
    val opQs = List((AND, Term("ocean")), (OR, Term("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        Term("the"),
        And(Term("cat"), Term("ocean")),
        Or(Term("fish")),
      )
    assertEquals(result, expected)
  }

  test("associates with two ORs and then AND") {
    // the cat OR ocean OR ocean2 AND fish
    // default:the default:cat default:ocean +default:ocean2 +default:fish
    val leftQs = NonEmptyList.of(Term("the"), Term("cat"))
    val opQs = List((OR, Term("ocean")), (OR, Term("ocean2")), (AND, Term("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        Term("the"),
        Term("cat"),
        Term("ocean"),
        And(Term("ocean2"), Term("fish")),
      )
    assertEquals(result, expected)
  }

  test("associates with two ANDs and then OR") {
    // the cat AND ocean AND ocean2 OR fish
    // default:the +default:cat +default:ocean +default:ocean2 default:fish
    val leftQs = NonEmptyList.of(Term("the"), Term("cat"))
    val opQs = List((AND, Term("ocean")), (AND, Term("ocean2")), (OR, Term("fish")))
    val result = associateOps(leftQs, opQs)
    val expected =
      NonEmptyList.of(
        Term("the"),
        And(Term("cat"), Term("ocean"), Term("ocean2")),
        Or(Term("fish")),
      )
    assertEquals(result, expected)
  }
}
