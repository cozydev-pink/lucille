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

import pink.cozydev.lucille.Query._
import cats.syntax.all._

object QueryPrinter {
  def print(query: Query): String =
    query match {
      case q: MultiQuery => q.qs.map(print).mkString_(" ")
      case q: TermQuery => strTermQuery(q)
      case q: Or => q.qs.map(print).mkString_(" OR ")
      case q: And => q.qs.map(print).mkString_(" AND ")
      case q: Not => "NOT " + print(q.q)
      case q: Group => q.qs.map(print).mkString_("(", " ", ")")
      case q: UnaryPlus => "+" + print(q.q)
      case q: UnaryMinus => "-" + print(q.q)
      case q: MinimumMatch => q.qs.map(print).mkString_("(", " ", s")@${q.num}")
      case q: Field => q.field + ":" + print(q.q)
    }

  def strTermQuery(q: TermQuery): String =
    q match {
      case q: Term => q.str
      case q: Phrase => "\"" + q.str + "\""
      case q: Prefix => q.str + "*"
      case q: Proximity => "\"" + q.str + "\"~" + q.num
      case q: Fuzzy => s"${q.str}~${q.num.getOrElse("")}"
      case q: TermRegex => q.str // TODO slashes?
      case q: TermRange =>
        val lft = if (q.lowerInc) "{" else "["
        val rgt = if (q.upperInc) "}" else "]"
        val parts = lft :: q.lower.getOrElse("*") :: " TO " :: q.upper.getOrElse("*") :: rgt :: Nil
        parts.mkString
    }

}
