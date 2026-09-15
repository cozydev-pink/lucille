package pink.cozydev.lucille

import org.scalacheck.Gen

object generators {

  val word: Gen[String] =
    Gen.nonEmptyStringOf(Gen.alphaLowerChar)

  def term(text: Gen[String]): Gen[Query.Term] =
    text.map(Query.Term.apply)

  val plainTerm: Gen[Query.Term] =
    term(word)
}
