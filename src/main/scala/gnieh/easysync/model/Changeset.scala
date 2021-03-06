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
package model

import scala.annotation.tailrec

final case class Changeset[T](from: Int, chars: Seq[Characters[T]]) {

  val to: Int = chars.map(_.size).sum

  private def slice(from: Int, until: Int): Seq[Characters[T]] = {
    @tailrec
    def loop(idx: Int, chars: Seq[Characters[T]], acc: Seq[Characters[T]]): Seq[Characters[T]] =
      if (idx >= until)
        acc
      else chars match {
        case Seq(c, rest @ _*) if idx >= from && idx < until =>
          val taken = c.take(until - (idx - from))
          loop(idx + taken.size, rest, acc :+ taken)
        case Seq(c, rest @ _*) =>
          loop(idx + c.size, rest, acc)
        case Seq() =>
          acc
      }
    loop(0, chars, Nil)
  }

  def apply(d: Document[T]): Document[T] = {
    if (this.from != d.characters.size) {
      throw new SyncException(f"This changeset applies to documents of size ${this.from} but trying to apply to a document of size ${d.characters.size}")
    }
    Document(chars.foldLeft(Seq.empty[T]) {
      case (acc, Range(start, end)) => acc ++ d.characters.view(start, end)
      case (acc, Sequence(s))       => acc ++ s
    })
  }

  def andThen(that: Changeset[T]): Changeset[T] = {
    if (this.to != that.from) {
      throw new SyncException(f"Expecting changeset from ${this.to} characters but got one from ${that.from} characters")
    }
    val chars1 = that.chars.flatMap {
      case Range(from, until) => this.slice(from, until)
      case Sequence(cs)       => Seq(Sequence(cs))
    }
    Changeset(this.from, chars1).compact
  }

  def compact: Changeset[T] = {

    @tailrec
    def loop(chars: Seq[Characters[T]], lastChar: Option[Characters[T]], acc: Seq[Characters[T]]): Seq[Characters[T]] =
      chars match {
        case Seq() =>
          // done
          lastChar match {
            case Some(c) => acc :+ c
            case None    => acc
          }
        case Seq(r @ Range(start1, end1), rest @ _*) =>
          lastChar match {
            case Some(Range(start2, end2)) if end2 == start1 =>
              // merge adjacent ranges
              loop(rest, Some(Range(start2, end1)), acc)
            case Some(c) =>
              // some other character, add it to the accumulator and replace with current range
              loop(rest, Some(r), acc :+ c)
            case None =>
              loop(rest, Some(r), acc)
          }
        case Seq(s @ Sequence(s1), rest @ _*) =>
          lastChar match {
            case Some(Sequence(s2)) =>
              // merge both sequences
              val s3 = Sequence(s2 ++ s1)
              loop(rest, Some(s3), acc)
            case Some(c) =>
              // some other character, add it to the accumulator and replace with current sequence
              loop(rest, Some(s), acc :+ c)
            case None =>
              loop(rest, Some(s), acc)
          }
      }

    Changeset(from, loop(chars, None, Nil))
  }

  override def toString() =
    f"($from → $to)[${chars.mkString(", ")}]"

}

object Changeset {

  def empty[T]: Changeset[T] =
    identity(0)

  /** Creates a changeset from the empty document to the provided `doc`. */
  def fromDocument[T](doc: Document[T]): Changeset[T] =
    Changeset(0, Seq(Sequence(doc.characters)))

}

sealed trait Characters[+T] {
  def size: Int
  def take(n: Int): Characters[T]
}

final case class Range(start: Int, end: Int) extends Characters[Nothing] {

  @inline
  def size = end - start

  @inline
  def take(n: Int): Characters[Nothing] =
    Range(start, math.min(end, start + n))

  @inline
  def overlap(that: Range): Boolean =
    this.start < that.end && that.start < this.end

  @inline
  def intersect(that: Range): Range =
    Range(math.max(this.start, that.start), math.min(this.end, that.end))

  @inline
  def transpose(at: Int): Range =
    Range(at, end - start + at)

  override def toString =
    if (end - start == 1)
      start.toString
    else
      f"$start-$end"
}

final case class Sequence[T](elems: Seq[T]) extends Characters[T] {
  def size = elems.size
  def take(n: Int): Characters[T] =
    Sequence(elems.take(n))
  override def toString =
    elems.mkString
}
