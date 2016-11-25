/* Copyright (c) 2016 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gnieh.easysync

import scala.annotation.tailrec

package object model {

  def identity[T](n: Int): Changeset[T] =
    if (n > 0)
      Changeset(n, Seq(Range(0, n)))
    else
      Changeset(0, Seq())

  def follow[T](a: Changeset[T], b: Changeset[T]): Changeset[T] = {
    if (a.from != b.from) {
      throw new SyncException(f"Both changesets must apply to the same document length but here apply to ${a.from} and ${b.from}")
    }

    @tailrec
    def loop(aidx: Int, a: Seq[Characters[T]], b: Seq[Characters[T]], acc: Seq[Characters[T]]): Seq[Characters[T]] =
      (a, b) match {
        case (Seq(Sequence(elemsa), resta @ _*), Seq(sb @ Sequence(_), restb @ _*)) =>
          // new elements in both, retain size in first and add elements from second
          loop(aidx + elemsa.size, resta, restb, acc :+ Range(aidx, aidx + elemsa.size) :+ sb)

        case (Seq(Sequence(elemsa), resta @ _*), Seq(Range(_, _), _*)) =>
          // retain elements from first
          loop(aidx + elemsa.size, resta, b, acc :+ Range(aidx, aidx + elemsa.size))

        case (Seq(Range(_, _), _*), Seq(Sequence(elemsb), restb @ _*)) =>
          // insert elements from second
          loop(aidx, a, restb, acc :+ Sequence(elemsb))

        case (Seq(ra @ Range(starta, enda), resta @ _*), Seq(rb @ Range(startb, endb), restb @ _*)) =>
          // retain elements retained in both
          if (ra.overlap(rb)) {
            val retain = ra.intersect(rb)
            val resta1 =
              if (retain.end < enda)
                Range(retain.end, enda) +: resta
              else
                resta
            val restb1 =
              if (retain.end < endb)
                Range(retain.end, endb) +: restb
              else
                restb
            loop(aidx + retain.end - starta, resta1, restb1, acc :+ retain.transpose(aidx))
          } else if (enda <= startb) {
            // retain in a is strictly before retain in b, skip a
            loop(aidx + ra.size, resta, b, acc)
          } else {
            // retain in a is strictly after retain in b, skip b
            loop(aidx, a, restb, acc)
          }

        case (Seq(Sequence(s), rest @ _*), Seq()) =>
          // retain the inserted characters
          loop(aidx + s.size, rest, Seq(), acc :+ Range(aidx, aidx + s.size))

        case (Seq(r @ Range(_, _), rest @ _*), Seq()) =>
          // forget about it
          loop(aidx + r.size, rest, Seq(), acc)

        case (Seq(), Seq(Sequence(s), rest @ _*)) =>
          // insert characters
          loop(aidx, Seq(), rest, acc :+ Sequence(s))

        case (Seq(), Seq(Range(_, _), rest @ _*)) =>
          // forget about it
          loop(aidx, Seq(), rest, acc)

        case (Seq(), Seq()) =>
          // it is over
          acc
      }

    Changeset(a.to, loop(0, a.chars, b.chars, Nil))
  }

}
