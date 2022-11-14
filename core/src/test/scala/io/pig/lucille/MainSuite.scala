/*
 * Copyright 2022 Pig.io
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

package io.pig.lucille
import cats.data.NonEmptyList
import cats.parse.Parser.Error

class AssociateOpsSuite extends munit.FunSuite {
  // def associateOps(q1: NonEmptyList[Query], opQs: List[(Op, Query)]): NonEmptyList[Query] = {
  import Parser._

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

class SingleSimpleQuerySuite extends munit.FunSuite {
  import Parser._

  def assertSingleTerm(r: Either[Error, NonEmptyList[Query]], expected: Query) =
    assertEquals(r, Right(NonEmptyList.of(expected)))

  test("parse single term") {
    val r = parseQ("the")
    assertSingleTerm(r, TermQ("the"))
  }

  test("parse single term with trailing whitespace") {
    val r = parseQ("the   ")
    assertSingleTerm(r, TermQ("the"))
  }

  test("parse single term with leading whitespace") {
    val r = parseQ("  the")
    assertSingleTerm(r, TermQ("the"))
  }

  test("parse single term with trailing and leading whitespace") {
    val r = parseQ("  the      ")
    assertSingleTerm(r, TermQ("the"))
  }

  test("parse phrase term") {
    val r = parseQ("\"The cat jumped\"")
    assertSingleTerm(r, PhraseQ("The cat jumped"))
  }

  test("parse phrase term with leading and trailing whitespace") {
    val r = parseQ("  \"The cat jumped\"  ")
    assertSingleTerm(r, PhraseQ("The cat jumped"))
  }

  test("parse field query with term") {
    val r = parseQ("fieldName:cat")
    assertSingleTerm(r, FieldQ("fieldName", TermQ("cat")))
  }

  test("parse field query with term with leading and trailing whitespace") {
    val r = parseQ("  fieldName:cat  ")
    assertSingleTerm(r, FieldQ("fieldName", TermQ("cat")))
  }

  test("parse field query with phrase") {
    val r = parseQ("fieldName:\"The cat jumped\"")
    assertEquals(r, Right(NonEmptyList.of(FieldQ("fieldName", PhraseQ("The cat jumped")))))
  }

}

class MultiSimpleQuerySuite extends munit.FunSuite {
  import Parser._

  test("parse multiple terms completely") {
    val r = parseQ("The cat jumped")
    assertEquals(r, Right(NonEmptyList.of(TermQ("The"), TermQ("cat"), TermQ("jumped"))))
  }

  test("parse multiple terms with lots of spaces completely") {
    val r = parseQ("The cat   jumped   ")
    assertEquals(r, Right(NonEmptyList.of(TermQ("The"), TermQ("cat"), TermQ("jumped"))))
  }

  test("parse field query and terms completely") {
    val r = parseQ("fieldName:The cat jumped")
    assertEquals(
      r,
      Right(NonEmptyList.of(FieldQ("fieldName", TermQ("The")), TermQ("cat"), TermQ("jumped"))),
    )
  }

  test("parse proximity query completely") {
    val r = parseQ("\"derp lerp\"~3")
    assertEquals(r, Right(NonEmptyList.of(ProximityQ("derp lerp", 3))))
  }

  test("parse proximity with decimal does not parse") {
    val r = parseQ("\"derp lerp\"~3.2")
    assert(r.isLeft)
  }

  test("parse fuzzy term without number parses completely") {
    val r = parseQ("derp~")
    assertEquals(r, Right(NonEmptyList.of(FuzzyTerm("derp", None))))
  }

  test("parse fuzzy term with number parses completely") {
    val r = parseQ("derp~2")
    assertEquals(r, Right(NonEmptyList.of(FuzzyTerm("derp", Some(2)))))
  }

  test("parse fuzzy term with decimal does not parse") {
    val r = parseQ("derp~3.2")
    assert(r.isLeft)
  }
}

class QueryWithSuffixOpsSuite extends munit.FunSuite {
  import Parser._

  test("parse two term OR query completely") {
    val r = parseQ("derp OR lerp")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          OrQ(NonEmptyList.of(TermQ("derp"), TermQ("lerp")))
        )
      ),
    )
  }

  test("parse three term OR query completely") {
    val r = parseQ("derp OR lerp OR slerp")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          OrQ(NonEmptyList.of(TermQ("derp"), TermQ("lerp"), TermQ("slerp")))
        )
      ),
    )
  }

  test("parse simple term and phrase OR query completely") {
    val r = parseQ("derp OR \"lerp slerp\"")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          OrQ(NonEmptyList.of(TermQ("derp"), PhraseQ("lerp slerp")))
        )
      ),
    )
  }

  test("parse 'OR' as term should fail") {
    val r = parseQ("OR")
    assert(r.isLeft)
  }

  test("parse 'AND' as term should fail") {
    val r = parseQ("AND")
    assert(r.isLeft)
  }

  test("simpleQ parse term with trailing 'OR' should fail") {
    val r = simpleQ.parseAll("cat OR")
    assert(r.isLeft)
  }

  test("simpleQ parse term with trailing 'AND' should fail") {
    val r = simpleQ.parseAll("cat AND")
    assert(r.isLeft)
  }

  test("simpleQ parse term with trailing 'OR' and whitespace should fail") {
    val r = simpleQ.parseAll("cat OR ")
    assert(r.isLeft)
  }

  test("simpleQ parse term with trailing 'AND' and whitespace should fail") {
    val r = simpleQ.parseAll("cat AND ")
    assert(r.isLeft)
  }

  test("parse two term AND query completely") {
    val r = parseQ("derp AND lerp")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          AndQ(NonEmptyList.of(TermQ("derp"), TermQ("lerp")))
        )
      ),
    )
  }

  test("parse simple term and phrase AND query completely") {
    val r = parseQ("derp AND \"lerp slerp\"")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          AndQ(NonEmptyList.of(TermQ("derp"), PhraseQ("lerp slerp")))
        )
      ),
    )
  }

  test("parse AND query with '&&' completely") {
    val r = parseQ("derp && \"lerp slerp\"")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          AndQ(NonEmptyList.of(TermQ("derp"), PhraseQ("lerp slerp")))
        )
      ),
    )
  }

  // TODO Confirm and possibly fix
  test("parse NOT query completely fails".fail) {
    val r = parseQ("derp AND NOT lerp")
    assertEquals(
      r,
      Right(
        NonEmptyList.of(
          AndQ(NonEmptyList.of(TermQ("derp"), NotQ(TermQ("lerp"))))
        )
      ),
    )
  }
}
