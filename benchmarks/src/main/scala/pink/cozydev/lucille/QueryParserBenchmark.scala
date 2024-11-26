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

/** To run the benchmark from within sbt:
  *
  * jmh:run -i 10 -wi 10 -f 2 -t 1 pink.cozydev.lucille.benchmarks.QueryParserBenchmark
  *
  * Which means "10 iterations", "10 warm-up iterations", "2 forks", "1 thread". Please note that
  * benchmarks should be usually executed at least in 10 iterations (as a rule of thumb), but
  * more is better.
  */
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class QueryParserBenchmark {

  val orQueries10: String = "o 1 2 3 4 5 6 7 8 9"
  var orQueries1000: String = _
  val associativityQueries = Vector(
    "NOT a AND b",
    "a AND NOT b",
    "a AND b OR x",
    "a AND b OR x AND y",
    "a AND b AND c OR x",
    "a b AND c",
    "a b AND c d",
    "a b AND c AND d",
    "a b AND c AND d AND e",
    "a b AND c AND d OR e",
    "a b AND c OR d e",
    "a b AND c OR d AND e",
    "a b AND c OR d OR e",
  )

  @Setup
  def setup(): Unit =
    orQueries1000 = (1 to 1000).mkString("o ", " ", "")

  @Benchmark
  def orQueries10Parse(): Either[String, Query] =
    QueryParser.default.parse(orQueries10)

  @Benchmark
  def orQueries1000Parse(): Either[String, Query] =
    QueryParser.default.parse(orQueries1000)

  @Benchmark
  def associativityQueriesParse(): Unit =
    associativityQueries.foreach(QueryParser.default.parse)

}
