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

import cats.implicits._
import PagedStringStream._

private[parsecat] final case class PagedStringStream(stream: Stream[Array[Char]],
                                                     pageOffset: Long,
                                                     isSinglePage: Boolean) {

  def char(offset: Long): Either[String, (Char, PagedStringStream)] = {
    if (offset < pageOffset) {
      "offset can't be smaller than the current stream position".asLeft
    } else if (isEmpty) {
      "unexpected end of input".asLeft
    } else {
      val current = stream.head
      val localOffset = (offset - pageOffset).toInt
      if (localOffset >= current.length) {
        nextPage.char(offset)
      } else {
        (current(localOffset), this).asRight
      }
    }
  }

  def stringOfLength(length: Int, offset: Long): Either[String, (CharSequence, PagedStringStream)] = {
    if (offset < pageOffset) {
      "offset can't be smaller than the current stream position".asLeft
    } else if (isEmpty) {
      "unexpected end of input".asLeft
    } else {
      val current = stream.head
      val localOffset = (offset - pageOffset).toInt
      if (localOffset >= current.length) {
        nextPage.stringOfLength(length, offset)
      } else {
        val currentSlice = SlicedCharSequence(current, localOffset, localOffset + length)
        if (currentSlice.length < length) {
          val nextPageOffset = pageOffset + current.length
          val nextResult = nextPage.stringOfLength(length - currentSlice.length, nextPageOffset)
          nextResult match {
            case Right((slice, page)) => (CompositeCharSequence(currentSlice, slice), page).asRight
            case e @ Left(_) => e
          }
        } else {
          (currentSlice, this).asRight
        }
      }
    }
  }

  def pageRemainder(offset: Long): SlicedCharSequence = {
    val page = stream.head
    SlicedCharSequence(page, (offset - pageOffset).toInt, page.length)
  }

  def isEmpty: Boolean = stream.isEmpty

  def nextPage: PagedStringStream = {
    PagedStringStream(stream.tail, pageOffset + stream.head.length, isSinglePage)
  }
}

object PagedStringStream {
  val PageSize = 4096

  def apply(stream: Stream[Array[Char]], localOffset: Long, isSinglePage: Boolean): PagedStringStream = {
    new PagedStringStream(stream, localOffset, isSinglePage)
  }

  implicit def fromString(str: String): PagedStringStream = {
    PagedStringStream(str.toCharArray #:: Stream.empty[Array[Char]], 0, true)
  }

  implicit def fromCharArray(a: Array[Char]): PagedStringStream = {
    PagedStringStream(a #:: Stream.empty[Array[Char]], 0, true)
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
    PagedStringStream(toStream(r), 0, false)
  }

  implicit def fromInputStream(s: InputStream): PagedStringStream = {
    fromReader(new InputStreamReader(s))
  }

  implicit def fromStringIterator(i: Iterator[String]): PagedStringStream = {
    def toStream(i: Iterator[String]): Stream[Array[Char]] = {
      val head = if (i.hasNext) Some(i.next().toCharArray) else None
      head.map(h => h #:: toStream(i)).getOrElse(Stream.empty)
    }
    PagedStringStream(toStream(i), 0, false)
  }

  implicit def fromCharArrayIterator(i: Iterator[Array[Char]]): PagedStringStream = {
    def toStream(i: Iterator[Array[Char]]): Stream[Array[Char]] = {
      val head = if (i.hasNext) Some(i.next()) else None
      head.map(h => h #:: toStream(i)).getOrElse(Stream.empty)
    }
    PagedStringStream(toStream(i), 0, false)
  }

  implicit def fromStringIterable(i: Iterable[String]): PagedStringStream = fromStringIterator(i.iterator)

  implicit def fromCharArrayIterable(i: Iterable[Array[Char]]): PagedStringStream = fromCharArrayIterator(i.iterator)

  final case class SlicedCharSequence(original: Array[Char], startIdx: Int, endIdx: Int) extends CharSequence {
    override def length(): Int = Math.min(endIdx, original.length) - startIdx

    override def subSequence(start: Int, end: Int): CharSequence =
      SlicedCharSequence(original, start + startIdx, end + startIdx)

    override def charAt(index: Int): Char = original(startIdx + index)

    override def toString: String = original.slice(startIdx, endIdx).mkString
  }

  final case class CompositeCharSequence(first: CharSequence, second: CharSequence) extends CharSequence {
    override def length(): Int = first.length() + second.length()

    override def subSequence(start: Int, end: Int): CharSequence = {
      if (start >= first.length()) {
        second.subSequence(start - first.length(), end - first.length())
      } else {
        if (end > first.length()) {
          first.subSequence(start, first.length()).toString + second.subSequence(0, end - first.length()).toString
        } else {
          first.subSequence(start, end)
        }
      }
    }

    override def charAt(index: Int): Char = {
      if (index >= first.length()) {
        second.charAt(index - first.length())
      } else {
        first.charAt(index)
      }
    }

    override def toString: String = first.toString + second.toString
  }
}
