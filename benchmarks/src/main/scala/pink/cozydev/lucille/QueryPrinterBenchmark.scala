package pink.cozydev.lucille
package benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import pink.cozydev.lucille.Query
import pink.cozydev.lucille.QueryPrinter
import pink.cozydev.lucille.QueryParser

/** To run the benchmark from within sbt:
  *
  * jmh:run -i 10 -wi 10 -f 2 -t 1 pink.cozydev.lucille.benchmarks.QueryPrinterBenchmark
  *
  * Which means "10 iterations", "10 warm-up iterations", "2 forks", "1 thread". Please note that
  * benchmarks should be usually executed at least in 10 iterations (as a rule of thumb), but
  * more is better.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class QueryPrinterBenchmark {

  var queries: List[Query] = _
  @Setup
  def setup(): Unit = {
    val qs = List(
      "hi hello this is a large multi term query with several different words",
      "one AND two AND three OR four OR five AND six OR seven AND eight OR nine AND ten",
      "(multiple terms)@2 OR title:\"The Title\" AND age:[10 TO 30}",
      "something AND ((multiple terms)@2 OR title:\"The Title\" AND age:[10 TO 30}) OR fuzzy~2",
    )
    queries = qs.map(QueryParser.parse).map(_.toOption.get)
  }


  @Benchmark
  def printer(): List[String] =
    queries.map(QueryPrinter.print)

}
