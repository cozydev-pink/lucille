package pink.cozydev.lucille

import org.scalacheck.Gen

object generators {

  val bool: Gen[Boolean] =
    Gen.oneOf(true, false)

  val word: Gen[String] =
    Gen.nonEmptyStringOf(Gen.alphaLowerChar)

  val phraseText: Gen[String] =
    Gen.nonEmptyListOf(word).map(_.mkString(" "))

  val nonNegativeInt: Gen[Int] =
    Gen.chooseNum(0, Int.MaxValue)

  val rangeBound: Gen[Option[String]] =
    Gen.option(Gen.oneOf(word, nonNegativeInt.map(_.toString())))

  val regexPattern: Gen[String] =
    word.flatMap(w => Gen.oneOf(w, s"$w.*", s"$w[0-9]+"))

  val wildCardOp: Gen[Query.WildCardOp] =
    Gen.oneOf(Query.WildCardOp.SingleChar, Query.WildCardOp.ManyChar)

  def term(text: Gen[String]): Gen[Query.Term] =
    text.map(Query.Term.apply)

  def phrase(text: Gen[String]): Gen[Query.Phrase] =
    text.map(Query.Phrase.apply)

  def prefix(text: Gen[String]): Gen[Query.Prefix] =
    text.map(Query.Prefix.apply)

  def proximity(text: Gen[String], num: Gen[Int]): Gen[Query.Proximity] =
    text.flatMap(t => num.flatMap(n => Query.Proximity(t, n)))

  def fuzzy(text: Gen[String], num: Gen[Option[Int]]): Gen[Query.Fuzzy] =
    text.flatMap(t => num.flatMap(n => Query.Fuzzy(t, n)))

  val plainTerm: Gen[Query.Term] = term(word)
  val plainPhrase: Gen[Query.Phrase] = phrase(phraseText)
  val plainPrefix: Gen[Query.Prefix] = prefix(word)
  val plainProximity: Gen[Query.Proximity] = proximity(phraseText, nonNegativeInt)
  val plainFuzzy: Gen[Query.Fuzzy] = fuzzy(word, Gen.option(nonNegativeInt))
}
