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

package pink.cozydev.lucille.internal
import pink.cozydev.lucille.Query._
import Op._

class AssociateOpsSuite extends munit.FunSuite {

  test("associates with one AND-Query pair") {
    val opQs = List((AND, Term("dogs")))
    val result = associateOps(Term("cats"), opQs)
    val expected = And(Term("cats"), Term("dogs"))
    assertEquals(result, expected)
  }

  test("associates with two AND-Query pairs") {
    val opQs = List((AND, Term("dogs")), (AND, Term("fish")))
    val result = associateOps(Term("cats"), opQs)
    val expected = And(Term("cats"), Term("dogs"), Term("fish"))
    assertEquals(result, expected)
  }

  test("associates with one OR-Query pair") {
    val opQs = List((OR, Term("dogs")))
    val result = associateOps(Term("cats"), opQs)
    val expected = Or(Term("cats"), Term("dogs"))
    assertEquals(result, expected)
  }

  test("associates with two OR-Query pairs") {
    val opQs = List((OR, Term("dogs")), (OR, Term("fish")))
    val result = associateOps(Term("cats"), opQs)
    val expected = Or(Term("cats"), Term("dogs"), Term("fish"))
    assertEquals(result, expected)
  }

  test("associates with two ANDs and then OR") {
    val opQs = List((AND, Term("ocean")), (AND, Term("ocean2")), (OR, Term("fish")))
    val result = associateOps(Term("cat"), opQs)
    val expected = Or(And(Term("cat"), Term("ocean"), Term("ocean2")), Term("fish"))
    assertEquals(result, expected)
  }

  test("associates with two ORs and then AND") {
    val opQs = List((OR, Term("ocean")), (OR, Term("ocean2")), (AND, Term("fish")))
    val result = associateOps(Term("cat"), opQs)
    val expected = Or(Term("cat"), Term("ocean"), And(Term("ocean2"), Term("fish")))
    assertEquals(result, expected)
  }

  test("associates with one AND-island") {
    val opQs =
      List((OR, Term("ocean")), (OR, Term("coast")), (AND, Term("island")), (OR, Term("ocean")))
    val result = associateOps(Term("cat"), opQs)
    val expected = Or(Term("cat"), Term("ocean"), And(Term("coast"), Term("island")), Term("ocean"))
    assertEquals(result, expected)
  }

  test("associates with two AND-islands") {
    val opQs =
      List((OR, Term("ocean")), (OR, Term("coast")), (AND, Term("island")), (OR, Term("ocean")))
    val double = opQs ++ opQs
    val result = associateOps(Term("cat"), double)
    val oceanQs = List(Term("ocean"), And(Term("coast"), Term("island")), Term("ocean"))
    val expected = Or(Term("cat"), oceanQs.head, oceanQs.tail ++ oceanQs)
    assertEquals(result, expected)
  }
}
