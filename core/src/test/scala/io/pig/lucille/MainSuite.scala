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

class ParserSuite extends munit.FunSuite {
  import Parser.query

  test("parse single term completely") {
    val r = query.parseAll("the")
    assert(r.isRight)
  }

  test("parse multiple terms completely") {
    val r = query.parseAll("The cat jumped")
    assert(r.isRight)
  }

  test("parse phrase term completely") {
    val r = query.parseAll("\"The cat jumped\"")
    assert(r.isRight)
  }

  test("parse field query with term completely") {
    val r = query.parseAll("fieldName:cat")
    assert(r.isRight)
  }

  test("parse field query with phrase completely") {
    val r = query.parseAll("fieldName:\"The cat jumped\"")
    assert(r.isRight)
  }

  test("parse field query and terms completely") {
    val r = query.parseAll("fieldName:The cat jumped")
    assert(r.isRight)
  }

  test("parse proximity query completely") {
    val r = query.parseAll("\"derp lerp\"~3")
    assert(r.isRight)
  }

  test("parse proximity with decimal does not parse") {
    val r = query.parseAll("\"derp lerp\"~3.2")
    assert(r.isLeft)
  }

  test("parse fuzzy term without number parses completely") {
    val r = query.parseAll("derp~")
    assert(r.isRight)
  }

  test("parse fuzzy term with number parses completely") {
    val r = query.parseAll("derp~2")
    assert(r.isRight)
  }

  test("parse fuzzy term with decimal does not parse") {
    val r = query.parseAll("derp~3.2")
    assert(r.isLeft)
  }

  test("parse two term OR query completely") {
    val r = query.parseAll("derp OR lerp")
    assert(r.isRight)
  }

  test("parse simple term and phrase OR query completely") {
    val r = query.parse("derp OR \"lerp slerp\"")
    assert(r.isRight)
  }

  test("parse two term AND query completely") {
    val r = query.parseAll("derp AND lerp")
    assert(r.isRight)
  }

  test("parse simple term and phrase AND query completely") {
    val r = query.parse("derp AND \"lerp slerp\"")
    assert(r.isRight)
  }

  test("parse AND query with '&&' completely") {
    val r = query.parse("derp && \"lerp slerp\"")
    assert(r.isRight)
  }
}
