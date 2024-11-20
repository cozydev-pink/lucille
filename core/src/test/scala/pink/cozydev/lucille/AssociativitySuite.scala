package pink.cozydev.lucille

import Query._
import munit.TestOptions

class AssociativitySuite extends munit.FunSuite {

  // Using `TestOptions` like this let's us still use `.only` on the test input/name
  def check(parser: QueryParser, prefix: String)(
      options: TestOptions,
      expected: Query,
  )(implicit loc: munit.Location): Unit = {
    val queryString = options.name
    val testName = s"$prefix '$queryString'"
    test(options.withName(testName)) {
      val actual = parser.parse(queryString)
      assertEquals(actual, Right(expected))
    }
  }

  val checkWithDefaultOr = check(QueryParser.default, "default OR :")
  val checkWithDefaultAnd = check(QueryParser.withDefaultOperatorAND, "default And:")

  checkWithDefaultOr(
    "a b AND c",
    Or(Term("a"), And(Term("b"), Term("c"))),
  )
  checkWithDefaultAnd(
    "a b AND c",
    And(Term("a"), And(Term("b"), Term("c"))),
  )

  checkWithDefaultOr(
    "a b AND c d",
    Or(Term("a"), And(Term("b"), Term("c")), Term("d")),
  )
  checkWithDefaultAnd(
    "a b AND c d",
    And(Term("a"), And(Term("b"), Term("c")), Term("d")),
  )

  checkWithDefaultOr(
    "a b AND c AND d",
    Or(Term("a"), And(Term("b"), Term("c"), Term("d"))),
  )
  checkWithDefaultAnd(
    "a b AND c AND d",
    And(Term("a"), And(Term("b"), Term("c"), Term("d"))),
  )

  checkWithDefaultOr(
    "a b AND c AND d AND e",
    Or(Term("a"), And(Term("b"), Term("c"), Term("d"), Term("e"))),
  )
  checkWithDefaultAnd(
    "a b AND c AND d AND e",
    And(Term("a"), And(Term("b"), Term("c"), Term("d"), Term("e"))),
  )

  checkWithDefaultOr(
    "a b AND c AND d OR e",
    Or(Term("a"), Or(And(Term("b"), Term("c"), Term("d")), Term("e"))),
  )
  checkWithDefaultAnd(
    "a b AND c AND d OR e",
    And(Term("a"), Or(And(Term("b"), Term("c"), Term("d")), Term("e"))),
  )

  checkWithDefaultOr(
    "a b AND c OR d e",
    Or(Term("a"), Or(And(Term("b"), Term("c")), Term("d")), Term("e")),
  )
  checkWithDefaultAnd(
    "a b AND c OR d e",
    And(Term("a"), Or(And(Term("b"), Term("c")), Term("d")), Term("e")),
  )

  checkWithDefaultOr(
    "a b AND c OR d AND e",
    Or(Term("a"), Or(And(Term("b"), Term("c")), And(Term("d"), Term("e")))),
  )
  checkWithDefaultAnd(
    "a b AND c OR d AND e",
    And(Term("a"), Or(And(Term("b"), Term("c")), And(Term("d"), Term("e")))),
  )

  checkWithDefaultOr(
    "a b AND c OR d OR e",
    Or(Term("a"), Or(And(Term("b"), Term("c")), Term("d"), Term("e"))),
  )
  checkWithDefaultAnd(
    "a b AND c OR d OR e",
    And(Term("a"), Or(And(Term("b"), Term("c")), Term("d"), Term("e"))),
  )

}
