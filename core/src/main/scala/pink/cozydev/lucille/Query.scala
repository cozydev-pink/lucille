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

import cats.data.NonEmptyList

sealed trait Query extends Product with Serializable

object Query {
  final case class TermQ(q: String) extends Query
  final case class PhraseQ(q: String) extends Query
  final case class FieldQ(field: String, q: Query) extends Query
  final case class ProximityQ(q: String, num: Int) extends Query
  final case class PrefixTerm(q: String) extends Query
  final case class Regex(r: String) extends Query
  final case class FuzzyTerm(q: String, num: Option[Int]) extends Query
  final case class OrQ(qs: NonEmptyList[Query]) extends Query
  final case class AndQ(qs: NonEmptyList[Query]) extends Query
  final case class NotQ(q: Query) extends Query
  final case class Group(qs: NonEmptyList[Query]) extends Query
  final case class UnaryPlus(q: Query) extends Query
  final case class UnaryMinus(q: Query) extends Query
  final case class RangeQ(
      lower: Option[String],
      upper: Option[String],
      lowerInc: Boolean,
      upperInc: Boolean,
  ) extends Query
}
