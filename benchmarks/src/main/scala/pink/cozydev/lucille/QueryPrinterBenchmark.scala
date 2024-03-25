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
package benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import cats.data.NonEmptyList

/** To run the benchmark from within sbt:
  *
  * jmh:run -i 10 -wi 10 -f 2 -t 1 pink.cozydev.lucille.benchmarks.QueryPrinterBenchmark
  *
  * Which means "10 iterations", "10 warm-up iterations", "2 forks", "1 thread". Please note that
  * benchmarks should be usually executed at least in 10 iterations (as a rule of thumb), but
  * more is better.
  */
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class QueryPrinterBenchmark {
  import Query._

  var orQueries10: Query = _
  var orQueries1000: Query = _
  var queries: Vector[Query] = Vector.empty

  @Setup
  def setup(): Unit = {
    orQueries10 = Or(NonEmptyList(Term("o"), (1 to 10).map(i => Term(i.toString)).toList))
    orQueries1000 = Or(NonEmptyList(Term("o"), (1 to 1000).map(i => Term(i.toString)).toList))
    queries = Vector(
      Term("term"),
      Phrase("phrase query"),
      Prefix("prefi"),
      Proximity("proximity query", 2),
      Fuzzy("fuzzy", None),
      Fuzzy("fuzzy", Some(2)),
      TermRegex("/.ump(s|ing)/"),
      TermRange(None, None, false, false),
      TermRange(Some("apple"), None, true, false),
      TermRange(None, Some("banana"), false, true),
      TermRange(Some("apple"), Some("banana"), true, true),
    )
  }

  @Benchmark
  def orQueries10Print(): String =
    QueryPrinter.print(orQueries10)

  @Benchmark
  def orQueries1000Print(): String =
    QueryPrinter.print(orQueries1000)

  @Benchmark
  def termQueriesPrint(): Vector[String] =
    queries.map(q => QueryPrinter.print(q))

}
