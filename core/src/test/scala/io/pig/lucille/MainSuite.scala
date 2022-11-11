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
}
