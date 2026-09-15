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

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class RoundtripSuite extends ScalaCheckSuite {

  private def assertRoundTrips(query: Query)(implicit loc: munit.Location): Unit = {
    val printed = QueryPrinter.print(query)
    val parsed = QueryParser.parse(printed)
    assertEquals(parsed, Right(query), s"printed query: $printed")
  }

  property("Query.Term printing roundtrips") {
    forAll(generators.plainTerm)(q => assertRoundTrips(q))
  }

  property("Query.Phrase printing roundtrips") {
    forAll(generators.plainPhrase)(q => assertRoundTrips(q))
  }

  property("Query.Prefix printing roundtrips") {
    forAll(generators.plainPrefix)(q => assertRoundTrips(q))
  }

  property("Query.Proximity printing roundtrips") {
    forAll(generators.plainProximity)(q => assertRoundTrips(q))
  }

  property("Query.Fuzzy printing roundtrips") {
    forAll(generators.plainFuzzy)(q => assertRoundTrips(q))
  }

  property("Query.TermRegex printing roundtrips") {
    forAll(generators.plainRegex)(q => assertRoundTrips(q))
  }

  property("Query.TermRange printing roundtrips") {
    forAll(generators.plainRange)(q => assertRoundTrips(q))
  }

}
