/*
 * Copyright 2022 Cozydev.pink
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

sealed trait Op extends Product with Serializable

object Op {
  case object OR extends Op
  case object AND extends Op

  /** @param q1 queries parsed so far, the last one could be part of a suffixOp
    * @param qs suffixOp and query pairs
    * @return
    */
  def associateOps(q1: NonEmptyList[Query], opQs: List[(Op, Query)]): NonEmptyList[Query] = {
    def go(acc: NonEmptyList[Query], op: Op, opQs: List[(Op, Query)]): NonEmptyList[Query] =
      opQs match {
        case Nil =>
          op match {
            // no more ops to pair
            case OR => NonEmptyList.of(Query.OrQ(acc))
            case AND => NonEmptyList.of(Query.AndQ(acc))
          }
        case (nextOp, q) :: tailOpP =>
          (op, nextOp) match {
            case (OR, OR) => go(acc.append(q), nextOp, tailOpP)
            case (AND, AND) => go(acc.append(q), nextOp, tailOpP)
            case (AND, OR) =>
              go(NonEmptyList.of(q), nextOp, tailOpP).prepend(Query.AndQ(acc))
            case (OR, AND) =>
              // TODO we only get away with not wrapping the `allButLast` in an OrQ
              //  because `OR` is the default query type. This should be configurable
              val allButLast = NonEmptyList(acc.head, acc.tail.dropRight(1))
              allButLast.concatNel(go(NonEmptyList.of(acc.last, q), nextOp, tailOpP))
          }
      }

    opQs match {
      case Nil => q1
      case opHead :: _ =>
        q1 match {
          case NonEmptyList(_, Nil) => go(q1, opHead._1, opQs)
          case NonEmptyList(h, atLeastOneQ) =>
            // multiple queries on the left, we'll look at just the last one
            val allButLast = NonEmptyList(h, atLeastOneQ.dropRight(1))
            allButLast.concatNel(go(NonEmptyList.of(q1.last), opHead._1, opQs))
        }
    }
  }

}
