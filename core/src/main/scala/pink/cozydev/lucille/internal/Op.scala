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
import cats.parse.Accumulator
import cats.parse.Appender
import scala.collection.mutable.ListBuffer

private[lucille] sealed trait Op extends Product with Serializable

private[lucille] object Op {
  case object OR extends Op
  case object AND extends Op

  implicit def allButLastAccumulator0[A]: Accumulator[A, (List[A], A)] =
    new Accumulator[A, (List[A], A)] {
      def newAppender(first: A): Appender[A, (List[A], A)] =
        new Appender[A, (List[A], A)] {
          var last = first
          val bldr = List.newBuilder[A]
          def append(item: A) = {
            bldr += last
            last = item
            this
          }

          def finish() = (bldr.result(), last)
        }
    }

  /** Associates a starting query and a list of OP-Query pairs.
    *
    * @param first First query in sequence of 'firstQ OP query OP query'
    * @param opQs List of OP-Query pairs
    * @return A Single top level `Or`/`And` query
    */
  def associateOps(first: Query, opQs: List[(Op, Query)]): Query =
    opQs match {
      case Nil => first
      case (headOp, headQ) :: remaining =>
        var currentOp = headOp
        var currentQ = headQ

        // We'll collect queries in 'tempAccumulator' while successive OPs are the same type
        val tempAccumulator = ListBuffer.empty[Query]
        tempAccumulator += first

        // When successive OPs change type, we clear 'tempAccumulator' and add them to 'outerBlder'
        val outerBlder = ListBuffer.empty[Query]

        // Iterate through OP-Query pairs, looking "one ahead" to deside how to process 'currentQ'
        remaining.foreach { case (nextOp, nextQ) =>
          if (currentOp == nextOp) {
            // nextOp hasn't changed, keep accumulating
            tempAccumulator += currentQ
          } else {
            // 'nextOp' is different from 'currentOp', so we're going to collapse the queries we've
            // accumulated so far into an AND/OR query before continuing.
            // How we do that depends on the precedence of the operator we're switching to
            // if we are switching to AND, it has higher precedence than OR, so we collapse before
            // accumulating 'currentQ' and instead add it to the newly cleared accumulator.
            nextOp match {
              case AND =>
                // OR -> AND
                // previousQ OR (currentQ AND nextQ)
                // From OR to AND, collapse now, new AND gets currentQ
                val qs = tempAccumulator.result()
                tempAccumulator.clear()
                outerBlder ++= qs
                tempAccumulator += currentQ
              case OR =>
                // AND -> OR
                // (previousQ AND currentQ) OR nextQ
                // From AND to OR, add currentQ before collapsing
                tempAccumulator += currentQ
                val qs = tempAccumulator.result()
                tempAccumulator.clear()

                outerBlder += Query.And.fromListUnsafe(qs)
            }
          }
          // get ready for next iteration
          currentOp = nextOp
          currentQ = nextQ
        }

        // We're done iterating
        // But because we were looking one ahead, we still have not processed the last 'currentQ'
        // Safe to add 'currentQ' to 'tempAccumulator', it's either collecting the same type of
        // queries, or we've just emptied it for this new type of query.
        tempAccumulator += currentQ
        val innerQs = tempAccumulator.result()
        currentOp match {
          case AND =>
            // Final OP was an AND, collapse into one AND query, add to outer
            outerBlder += Query.And.fromListUnsafe(innerQs)
          case OR =>
            // Final OP was an OR, directly add to outer
            outerBlder ++= innerQs
        }
        val outerQs = outerBlder.result()
        // If we only have one query, directly return that, otherwise wrap in OR
        if (outerQs.size == 1) outerQs.head else Query.Or.fromListUnsafe(outerQs)
    }

}
