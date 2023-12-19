package pink.cozydev.lucille
package benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import pink.cozydev.lucille.Query
import pink.cozydev.lucille.QueryPrinter
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

  @Param(Array("10", "100", "1000"))
  var size: Int = 0

  var orQueries: Query = _
  var queries: Vector[Query] = Vector.empty

  @Setup
  def setup(): Unit = {
    orQueries = Or(NonEmptyList(Term("o"), (1 to size).map(i => Term(i.toString)).toList))
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
  def orQueriesPrint(): String =
    QueryPrinter.print(orQueries)

  @Benchmark
  def termQueriesPrint(): Vector[String] =
    queries.map(QueryPrinter.print)

}
