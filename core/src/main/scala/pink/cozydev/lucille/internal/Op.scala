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

import pink.cozydev.lucille.Query
import scala.collection.mutable.ListBuffer
import cats.data.NonEmptyList

private[lucille] sealed trait Op extends Product with Serializable

private[lucille] object Op {
  case object OR extends Op
  case object AND extends Op

  /** Associates a starting query and a list of OP-Query pairs.
    *
    * @param first First query in sequence of 'firstQ OP query OP query'
    * @param opQs List of OP-Query pairs
    * @return A Single top level `Or`/`And` query
    */
  def associateOps(first: Query, opQs: List[(Op, Query)]): Query =
    opQs match {
      case Nil => first
      case (onlyOp, secondQ) :: Nil =>
        onlyOp match {
          case OR => Query.Or(NonEmptyList.one(first), secondQ)
          case AND => Query.And(NonEmptyList.one(first), secondQ)
        }
      case (headOp, headQ) :: remaining =>
        var lastOp = headOp
        var lastQ = headQ

        // We'll collect queries in 'tempAccumulator' while successive operators are the same type
        // e.g. (OR, q1), (OR, q2), (OR, q3), ...
        val tempAccumulator = ListBuffer.empty[Query]
        tempAccumulator += first

        // When successive operators change type, we clear 'tempAccumulator' and add them to 'bldr'
        // e.g. (OR, q1), (AND, q2), ...
        val bldr = ListBuffer.empty[Query]

        // Iterate through Op-Query pairs
        remaining.foreach { case (currentOp, currentQ) =>
          if (lastOp == currentOp) {
            // Op hasn't changed, keep accumulating
            tempAccumulator += lastQ
          } else {
            // 'currentOp' is different from 'lastOp'
            // Collapse accumulated queries so far into an AND/OR query before continuing.
            // How we do that depends on the precedence of the operator we're switching to.
            // AND has higher precedence than OR, so if we are switching from OR to AND, we
            // collapse before accumulating 'lastQ' and instead add it to the newly cleared
            // accumulator.
            currentOp match {
              case AND =>
                // OR -> AND
                // e.g. OR (lastQ AND currentQ)
                // From OR to AND, collapse now, new AND gets lastQ
                val qs = tempAccumulator.result()
                tempAccumulator.clear()
                bldr ++= qs
                tempAccumulator += lastQ
              case OR =>
                // AND -> OR
                // e.g. (... AND lastQ) OR currentQ
                // From AND to OR, add lastQ to AND query
                val qs = NonEmptyList.fromListUnsafe(tempAccumulator.result())
                tempAccumulator.clear()
                bldr += Query.And(qs, lastQ)
            }
          }
          // prep for next iteration
          lastQ = currentQ
          lastOp = currentOp
        }
        val qs = tempAccumulator.result()

        bldr.result() match {
          case Nil =>
            lastOp match {
              case OR => Query.Or(NonEmptyList.fromListUnsafe(qs), lastQ)
              case AND => Query.And(NonEmptyList.fromListUnsafe(qs), lastQ)
            }
          case head :: tail =>
            lastOp match {
              case OR => Query.Or(NonEmptyList(head, tail ++ qs), lastQ)
              case AND =>
                Query.Or(
                  NonEmptyList(head, tail),
                  Query.And(NonEmptyList.fromListUnsafe(qs), lastQ),
                )
            }
        }
    }

}
