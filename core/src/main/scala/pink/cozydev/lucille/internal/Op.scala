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

package pink.cozydev.lucille.internal

import cats.data.NonEmptyList
import pink.cozydev.lucille.Query

private[lucille] sealed trait Op extends Product with Serializable

private[lucille] object Op {
  case object OR extends Op
  case object AND extends Op

  /** @param q1 queries parsed so far, the last one could be part of a suffixOp
    * @param qs suffixOp and query pairs
    * @return
    */
  def associateOps(
      q1: NonEmptyList[Query],
      opQs: List[(Op, Query)],
      defaultBooleanOR: Boolean,
  ): NonEmptyList[Query] = {
    def go(acc: NonEmptyList[Query], op: Op, opQs: List[(Op, Query)]): NonEmptyList[Query] =
      opQs match {
        case Nil =>
          op match {
            // no more ops to pair
            case OR => NonEmptyList.of(Query.Or(acc))
            case AND => NonEmptyList.of(Query.And(acc))
          }
        case (nextOp, q) :: tailOpP =>
          (op, nextOp) match {
            case (OR, OR) => go(acc.append(q), nextOp, tailOpP)
            case (AND, AND) => go(acc.append(q), nextOp, tailOpP)
            case (AND, OR) =>
              go(NonEmptyList.of(q), nextOp, tailOpP).prepend(Query.And(acc))
            case (OR, AND) =>
              if (defaultBooleanOR) {
                val allButLast = NonEmptyList(acc.head, acc.tail.dropRight(1))
                allButLast.concatNel(go(NonEmptyList.of(acc.last, q), nextOp, tailOpP))
              } else go(NonEmptyList.of(q), nextOp, tailOpP).prepend(Query.And(acc))
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
