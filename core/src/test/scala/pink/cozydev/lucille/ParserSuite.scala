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

class SingleSimpleQuerySuite extends munit.FunSuite {

  def assertSingleTerm(r: Either[Error, MultiQuery], expected: Query)(implicit
      loc: munit.Location
  ) =
    assertEquals(r, Right(MultiQuery(expected)))

  test("parse single term") {
    val r = parseQ("the")
    assertSingleTerm(r, Term("the"))
  }

  test("parse single term with trailing whitespace") {
    val r = parseQ("the   ")
    assertSingleTerm(r, Term("the"))
  }

  test("parse single term with leading whitespace") {
    val r = parseQ("  the")
    assertSingleTerm(r, Term("the"))
  }

  test("parse single term with trailing and leading whitespace") {
    val r = parseQ("  the      ")
    assertSingleTerm(r, Term("the"))
  }

  test("parse phrase term") {
    val r = parseQ("\"The cat jumped\"")
    assertSingleTerm(r, Phrase("The cat jumped"))
  }

  test("parse phrase term with smart quotes") {
    val r = parseQ("“The cat jumped”")
    assertSingleTerm(r, Phrase("The cat jumped"))
  }

  test("parse phrase term with leading and trailing whitespace") {
    val r = parseQ("  \"The cat jumped\"  ")
    assertSingleTerm(r, Phrase("The cat jumped"))
  }

  test("parse field query with term") {
    val r = parseQ("fieldName:cat")
    assertSingleTerm(r, Field("fieldName", Term("cat")))
  }

  test("parse field query with term with leading and trailing whitespace") {
    val r = parseQ("  fieldName:cat  ")
    assertSingleTerm(r, Field("fieldName", Term("cat")))
  }

  test("parse field query with phrase") {
    val r = parseQ("fieldName:\"The cat jumped\"")
    assertEquals(r, Right(MultiQuery(Field("fieldName", Phrase("The cat jumped")))))
  }

  test("parse single term with numbers") {
    val r = parseQ("catch22")
    assertSingleTerm(r, Term("catch22"))
  }

  test("parse field query with number in name") {
    val r = parseQ("fieldName42:cat")
    assertSingleTerm(r, Field("fieldName42", Term("cat")))
  }

  test("parse field query with number in term") {
    val r = parseQ("fieldName42:cat42")
    assertSingleTerm(r, Field("fieldName42", Term("cat42")))
  }

  test("field names cannot be reserved suffix operators") {
    val r = parseQ("AND:cat")
    assert(r.isLeft)
  }

  test("field names cannot be quoted") {
    val r = parseQ("\"AND\":cat")
    assert(r.isLeft)
  }

  test("parse boost on term query") {
    val r = parseQ("cats^3.0")
    assertSingleTerm(r, Boost(Term("cats"), 3f))
  }

  test("parse multiple boost on term queries") {
    val r = parseQ("cats^3.0 AND dogs^2")
    assertSingleTerm(r, And(Boost(Term("cats"), 3f), Boost(Term("dogs"), 2f)))
  }

  test("parse boost on field query") {
    val r = parseQ("fieldName42:cat42^3")
    assertSingleTerm(r, Field("fieldName42", Boost(Term("cat42"), 3f)))
  }

  test("parse boost on field query in a group query") {
    val r = parseQ("(fieldName42:cat42^3)")
    assertSingleTerm(r, Group(Field("fieldName42", Boost(Term("cat42"), 3f))))
  }

  test("parse boost on field query using boost with decimal point") {
    val r = parseQ("fieldName42:cat42^3.1")
    assertSingleTerm(r, Field("fieldName42", Boost(Term("cat42"), 3.1f)))
  }

  test("parse boost on field query with group query") {
    val r = parseQ("fieldName42:(cats AND dogs)^20")
    assertSingleTerm(r, Field("fieldName42", Boost(Group(And(Term("cats"), Term("dogs"))), 20f)))
  }

  test("parse boost does not parse with trailing 'f' on boost") {
    // TODO the trailing `f` is parsing as another term and it should not
    val r = parseQ("fieldName42:cat42^3.1f")
    assert(r.isLeft)
  }

  test("parse boost does not parse with trailing 'd' on boost") {
    val r = parseQ("fieldName42:cat42^3.1d")
    assert(r.isLeft)
  }

}

class MultiSimpleQuerySuite extends munit.FunSuite {

  test("parse multiple terms completely") {
    val r = parseQ("The cat jumped")
    assertEquals(r, Right(MultiQuery(Term("The"), Term("cat"), Term("jumped"))))
  }

  test("parse multiple terms with lots of spaces completely") {
    val r = parseQ("The cat   jumped   ")
    assertEquals(r, Right(MultiQuery(Term("The"), Term("cat"), Term("jumped"))))
  }

  test("parse field query and terms completely") {
    val r = parseQ("fieldName:The cat jumped")
    assertEquals(
      r,
      Right(MultiQuery(Field("fieldName", Term("The")), Term("cat"), Term("jumped"))),
    )
  }

  test("parse proximity query completely") {
    val r = parseQ("\"derp lerp\"~3")
    assertEquals(r, Right(MultiQuery(Proximity("derp lerp", 3))))
  }

  test("parse proximity with decimal does not parse") {
    val r = parseQ("\"derp lerp\"~3.2")
    assert(r.isLeft)
  }

  test("parse fuzzy term without number parses completely") {
    val r = parseQ("derp~")
    assertEquals(r, Right(MultiQuery(Fuzzy("derp", None))))
  }

  test("parse fuzzy term with number parses completely") {
    val r = parseQ("derp~2")
    assertEquals(r, Right(MultiQuery(Fuzzy("derp", Some(2)))))
  }

  test("parse fuzzy term with decimal does not parse") {
    val r = parseQ("derp~3.2")
    assert(r.isLeft)
  }
}

class QueryWithSuffixOpsSuite extends munit.FunSuite {

  test("parse two term OR query completely") {
    val r = parseQ("derp OR lerp")
    assertEquals(
      r,
      Right(
        MultiQuery(
          Or(Term("derp"), Term("lerp"))
        )
      ),
    )
  }

  test("parse three term OR query completely") {
    val r = parseQ("derp OR lerp OR slerp")
    assertEquals(
      r,
      Right(
        MultiQuery(
          Or(Term("derp"), Term("lerp"), Term("slerp"))
        )
      ),
    )
  }

  test("parse simple term and phrase OR query completely") {
    val r = parseQ("derp OR \"lerp slerp\"")
    assertEquals(
      r,
      Right(
        MultiQuery(
          Or(Term("derp"), Phrase("lerp slerp"))
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

  test("parse term with trailing 'OR' should fail") {
    val r = parseQ("cat OR")
    assert(r.isLeft)
  }

  test("parse term with trailing 'AND' should fail") {
    val r = parseQ("cat AND")
    assert(r.isLeft)
  }

  test("parse term with trailing 'OR' and whitespace should fail") {
    val r = parseQ("cat OR ")
    assert(r.isLeft)
  }

  test("parse term with trailing 'AND' and whitespace should fail") {
    val r = parseQ("cat AND ")
    assert(r.isLeft)
  }

  test("parse two term AND query completely") {
    val r = parseQ("derp AND lerp")
    assertEquals(
      r,
      Right(
        MultiQuery(
          And(Term("derp"), Term("lerp"))
        )
      ),
    )
  }

  test("parse term followed by two term OR query") {
    val r = parseQ("term derp OR lerp")
    assertEquals(
      r,
      Right(
        MultiQuery(
          Term("term"),
          Or(Term("derp"), Term("lerp")),
        )
      ),
    )
  }

  test("parse two term OR query followed by term") {
    val r = parseQ("derp OR lerp slerp")
    assertEquals(
      r,
      Right(
        MultiQuery(
          Or(Term("derp"), Term("lerp")),
          Term("slerp"),
        )
      ),
    )
  }

  test("parse two term AND query followed by term") {
    val r = parseQ("derp AND lerp slerp")
    assertEquals(
      r,
      Right(
        MultiQuery(
          And(Term("derp"), Term("lerp")),
          Term("slerp"),
        )
      ),
    )
  }

  test("parse simple term and phrase AND query completely") {
    val r = parseQ("derp AND \"lerp slerp\"")
    assertEquals(
      r,
      Right(
        MultiQuery(
          And(Term("derp"), Phrase("lerp slerp"))
        )
      ),
    )
  }

  test("parse AND query with '&&' completely") {
    val r = parseQ("derp && \"lerp slerp\"")
    assertEquals(
      r,
      Right(
        MultiQuery(
          And(Term("derp"), Phrase("lerp slerp"))
        )
      ),
    )
  }

  test("parse complex mix of AND and OR queries with trailing terms") {
    val r = parseQ("derp AND lerp slerp orA OR orB last")
    assertEquals(
      r,
      Right(
        MultiQuery(
          And(Term("derp"), Term("lerp")),
          Term("slerp"),
          Or(Term("orA"), Term("orB")),
          Term("last"),
        )
      ),
    )
  }

  test("parse NOT query") {
    val r = parseQ("NOT derp")
    assertEquals(
      r,
      Right(
        MultiQuery(
          Not(Term("derp"))
        )
      ),
    )
  }

  test("parse NOT query inside AND query") {
    val r = parseQ("derp AND NOT lerp")
    assertEquals(
      r,
      Right(
        MultiQuery(
          And(Term("derp"), Not(Term("lerp")))
        )
      ),
    )
  }
}

class GroupQuerySuite extends munit.FunSuite {

  test("parse multiple terms in a group") {
    val r = parseQ("(The cat jumped)")
    assertEquals(
      r,
      Right(
        MultiQuery(Group(Term("The"), Term("cat"), Term("jumped")))
      ),
    )
  }

  test("parse multiple terms with trailing spaces in a group".fail) {
    val r = parseQ("(The cat   jumped   )")
    assertEquals(
      r,
      Right(
        MultiQuery(Group(Term("The"), Term("cat"), Term("jumped")))
      ),
    )
  }

  test("parse NOT of group query") {
    val r = parseQ("animals NOT (cats AND dogs)")
    assertEquals(
      r,
      Right(
        MultiQuery(
          Term("animals"),
          Not(
            Group(And(Term("cats"), Term("dogs")))
          ),
        )
      ),
    )
  }

  test("parse field query with group query") {
    val r = parseQ("title:(cats AND dogs)")
    assertEquals(
      r,
      Right(
        MultiQuery(
          Field(
            "title",
            Group(And(Term("cats"), Term("dogs"))),
          )
        )
      ),
    )
  }

  test("parse field query AND group query") {
    val r = parseQ("title:test AND (pass OR fail)")
    assertEquals(
      r,
      Right(
        MultiQuery(
          And(
            Field("title", Term("test")),
            Group(
              Or(Term("pass"), Term("fail"))
            ),
          )
        )
      ),
    )
  }

  test("parse nested group query with trailing term") {
    val r = parseQ("(title:test AND (pass OR fail)) extra")
    val gq = And(
      NonEmptyList.of(
        Field("title", Term("test")),
        Group(
          Or(Term("pass"), Term("fail"))
        ),
      )
    )
    assertEquals(
      r,
      Right(
        MultiQuery(Group(gq), Term("extra"))
      ),
    )
  }

  test("parse nested group query AND'd with a phrase query") {
    val r = parseQ("(title:test AND (pass OR fail)) AND \"extra phrase\"")
    val gq = And(
      NonEmptyList.of(
        Field("title", Term("test")),
        Group(
          Or(Term("pass"), Term("fail"))
        ),
      )
    )
    assertEquals(
      r,
      Right(
        MultiQuery(
          And(
            Group(gq),
            Phrase("extra phrase"),
          )
        )
      ),
    )
  }
}
