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
  * jmh:run -i 10 -wi 10 -f 2 -t 1 pink.cozydev.lucille.benchmarks.MapLastTermBenchmark
  *
  * Which means "10 iterations", "10 warm-up iterations", "2 forks", "1 thread". Please note that
  * benchmarks should be usually executed at least in 10 iterations (as a rule of thumb), but
  * more is better.
  */
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class MapLastTermBenchmark {

  val rewriteQ = (t: Query.Term) => Query.Or(t, Query.Prefix(t.str))

  val fullQuery: String =
    "this is a long query that will be broken up into one query per character in this string"
  val associativityQueryStrings = Vector(
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

  var partialQueries: Vector[Query] = _
  var associativityQueries: Vector[Query] = _

  @Setup
  def setup(): Unit =
    partialQueries = (1 to fullQuery.size)
      .map(fullQuery.take)
      .toVector
      .map(QueryParser.default.parse)
      .map(_.toOption.get)
  associativityQueries = associativityQueryStrings.map(QueryParser.parse).map(_.toOption.get)

  @Benchmark
  def partialQueriesMapLastTerm(): Unit =
    partialQueries.foreach(q => q.mapLastTerm(rewriteQ))

  @Benchmark
  def associativityQueriesMapLastTerm(): Unit =
    associativityQueries.foreach(q => q.mapLastTerm(rewriteQ))

}
