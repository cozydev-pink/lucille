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

class ParserSuite extends munit.FunSuite {
  import Parser._

  test("parse single term completely") {
    val r = parseQ("the")
    assertEquals(r, Right(NonEmptyList.of(TermQ("the"))))
  }

  test("parse single term with trailing whitespace completely") {
    val r = parseQ("the   ")
    assertEquals(r, Right(NonEmptyList.of(TermQ("the"))))
  }

  test("parse multiple terms completely") {
    val r = parseQ("The cat jumped")
    assertEquals(r, Right(NonEmptyList.of(TermQ("The"), TermQ("cat"), TermQ("jumped"))))
  }

  test("parse multiple terms with lots of spaces completely") {
    val r = parseQ("The cat   jumped   ")
    assertEquals(r, Right(NonEmptyList.of(TermQ("The"), TermQ("cat"), TermQ("jumped"))))
  }

  test("parse phrase term completely") {
    val r = parseQ("\"The cat jumped\"")
    assertEquals(r, Right(NonEmptyList.of(PhraseQ("The cat jumped"))))
  }

  test("parse field query with term completely") {
    val r = parseQ("fieldName:cat")
    assertEquals(r, Right(NonEmptyList.of(FieldQ("fieldName", TermQ("cat")))))
  }

  test("parse field query with phrase completely") {
    val r = parseQ("fieldName:\"The cat jumped\"")
    assertEquals(r, Right(NonEmptyList.of(FieldQ("fieldName", PhraseQ("The cat jumped")))))
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

  test("simpleQ parse term with trailing 'OR' should fail") {
    val r = simpleQ.parseAll("cat OR")
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
