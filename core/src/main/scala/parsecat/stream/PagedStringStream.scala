/*
 * Copyright (c) 2018 Iaroslav Zeigerman
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package parsecat.stream

import java.io.{InputStream, InputStreamReader, Reader}

import cats._
import cats.implicits._
import PagedStringStream._

private[parsecat] final class PagedStringStream(stream: Eval[Stream[Array[Char]]],
                                                localOffset: Int,
                                                val isSinglePage: Boolean) {

  def char(): Either[String, (Char, PagedStringStream)] = {
    getSlice(1).map { case (s, p) => (s(0), p) }
  }

  def stringOfLength(length: Int): Either[String, (String, PagedStringStream)] = {
    getSlice(length).map { case (s, p) => (s.mkString, p) }
  }

  def pageRemainder: SlicedCharSequence = {
    SlicedCharSequence(stream.value.head, localOffset)
  }

  def skip(length: Int): PagedStringStream = {
    PagedStringStream(stream, localOffset + length, isSinglePage)
  }

  def isEmpty: Boolean = stream.value.isEmpty

  private def getSlice(length: Int): Either[String, (Array[Char], PagedStringStream)] = {
    if (!isEmpty) {
      val current = stream.value.head
      if (localOffset >= current.length) {
        PagedStringStream(stream.map(_.tail), localOffset - current.length, isSinglePage).getSlice(length)
      } else {
        val currentSlice = current.slice(localOffset, localOffset + length)
        if (currentSlice.length < length) {
          val nextResult = PagedStringStream(stream.map(_.tail), 0, isSinglePage).getSlice(length - currentSlice.length)
          nextResult match {
            case Right((nextSlice, nextPage)) => (currentSlice ++ nextSlice, nextPage).asRight
            case e @ Left(_) => e
          }
        } else {
          (currentSlice, PagedStringStream(stream, localOffset + currentSlice.length, isSinglePage)).asRight
        }
      }
    } else {
      "unexpected end of input".asLeft
    }
  }
}

object PagedStringStream {
  val PageSize = 4096

  def apply(stream: Eval[Stream[Array[Char]]], localOffset: Int, isSinglePage: Boolean): PagedStringStream = {
    new PagedStringStream(stream, localOffset, isSinglePage)
  }

  implicit def fromString(str: String): PagedStringStream = {
    PagedStringStream(Eval.later(Stream(str.toCharArray)), 0, true)
  }

  implicit def fromCharArray(a: Array[Char]): PagedStringStream = {
    PagedStringStream(Eval.later(Stream(a)), 0, true)
  }

  implicit def fromReader(r: Reader): PagedStringStream = {
    def toStream(r: Reader): Stream[Array[Char]] = {
      val buffer = new Array[Char](PageSize)
      val head =
        if (r.read(buffer) >= 0) {
          Some(buffer)
        } else {
          None
        }
      head.map(h => h #:: toStream(r)).getOrElse(Stream.empty)
    }
    PagedStringStream(Eval.later(toStream(r)), 0, false)
  }

  implicit def fromInputStream(s: InputStream): PagedStringStream = {
    fromReader(new InputStreamReader(s))
  }

  final case class SlicedCharSequence(original: Array[Char], offset: Int) extends CharSequence {

    override def length(): Int = original.length - offset

    override def subSequence(start: Int, end: Int): CharSequence =
      original.subSequence(start + offset, end + offset)

    override def charAt(index: Int): Char = original(offset + index)

    override def toString: String = original.slice(offset, original.length).mkString
  }
}
