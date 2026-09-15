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

  def regex(pattern: Gen[String]): Gen[Query.TermRegex] =
    pattern.map(Query.TermRegex.apply)

  def range(
      lower: Gen[Option[String]],
      upper: Gen[Option[String]],
      lowerInc: Gen[Boolean],
      upperInc: Gen[Boolean],
  ): Gen[Query.TermRange] =
    for {
      l <- lower
      u <- upper
      li <- lowerInc
      ui <- upperInc
    } yield Query.TermRange(l, u, li, ui)

  val plainTerm: Gen[Query.Term] = term(word)
  val plainPhrase: Gen[Query.Phrase] = phrase(phraseText)
  val plainPrefix: Gen[Query.Prefix] = prefix(word)
  val plainProximity: Gen[Query.Proximity] = proximity(phraseText, nonNegativeInt)
  val plainFuzzy: Gen[Query.Fuzzy] = fuzzy(word, Gen.option(nonNegativeInt))
  val plainRegex: Gen[Query.TermRegex] = regex(regexPattern)
  val plainRange: Gen[Query.TermRange] = range(rangeBound, rangeBound, bool, bool)
}
