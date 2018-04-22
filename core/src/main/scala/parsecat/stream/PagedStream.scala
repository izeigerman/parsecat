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

import java.io.Reader

import cats.implicits._

private[parsecat] final case class PagedStream[A](stream: Stream[Array[A]],
                                                  pageOffset: Long,
                                                  isSinglePage: Boolean) {

  def apply(offset: Long): Either[String, (A, PagedStream[A])] = {
    applyF[A](offset, (stream, head, localOffset) => (head(localOffset), stream).asRight)
  }

  def slice(length: Int, offset: Long): Either[String, (SlicableSequence[A], PagedStream[A])] = {
    applyF[SlicableSequence[A]](offset, (stream, head, localOffset) => {
      val currentSlice = SlicedSequence(head, localOffset, localOffset + length)
      if (currentSlice.length < length) {
        val nextPageOffset = pageOffset + head.length
        val nextResult = stream.nextPage.slice(length - currentSlice.length, nextPageOffset)
        nextResult match {
          case Right((slice, page)) => (CompositeSlicableSequence(currentSlice, slice), page).asRight
          case e @ Left(_) => e
        }
      } else {
        (currentSlice, stream).asRight
      }
    })
  }

  def takeWhile(offset: Long, p: A => Boolean): Either[String, (SlicableSequence[A], PagedStream[A])] = {
    applyF[SlicableSequence[A]](offset, (stream, head, localOffset) => {
      var endIdx = localOffset
      while (endIdx < head.length && p(head(endIdx))) endIdx += 1
      val currentSlice = SlicedSequence(head, localOffset, endIdx)
      if (endIdx >= head.length) {
        val nPage = stream.nextPage
        if (!nPage.isEmpty) {
          val nextResult = nPage.takeWhile(pageOffset + head.length, p)
          nextResult match {
            case Right((slice, page)) => (CompositeSlicableSequence(currentSlice, slice), page).asRight
            case e @ Left(_) => e
          }
        } else {
          (currentSlice, stream).asRight
        }
      } else {
        (currentSlice, stream).asRight
      }
    })
  }

  def pageRemainder(offset: Long): SlicableSequence[A] = {
    val page = stream.head
    SlicedSequence(page, (offset - pageOffset).toInt, page.length)
  }

  def isEmpty: Boolean = stream.isEmpty

  def nextPage: PagedStream[A] = PagedStream(stream.tail, pageOffset + stream.head.length, isSinglePage)

  private def applyF[B](offset: Long, f: (PagedStream[A], Array[A], Int) => Either[String, (B, PagedStream[A])]): Either[String, (B, PagedStream[A])] = {
    if (offset < pageOffset) {
      "offset can't be smaller than the current stream position".asLeft
    } else if (isEmpty) {
      "unexpected end of input".asLeft
    } else {
      val head = stream.head
      val localOffset = (offset - pageOffset).toInt
      if (localOffset >= head.length) {
        nextPage.applyF(offset, f)
      } else {
        f(this, head, localOffset)
      }
    }
  }
}

object PagedStream {
  val PageSize = 4096

  implicit def fromString(str: String): PagedStream[Char] = {
    PagedStream(str.toCharArray #:: Stream.empty[Array[Char]], 0, true)
  }

  implicit def fromCharArray(a: Array[Char]): PagedStream[Char] = {
    PagedStream(a #:: Stream.empty[Array[Char]], 0, true)
  }

  implicit def fromReader(r: Reader): PagedStream[Char] = {
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
    PagedStream(toStream(r), 0, false)
  }

  implicit def fromStringIterator(i: Iterator[String]): PagedStream[Char] = {
    def toStream(i: Iterator[String]): Stream[Array[Char]] = {
      val head = if (i.hasNext) Some(i.next().toCharArray) else None
      head.map(h => h #:: toStream(i)).getOrElse(Stream.empty)
    }
    PagedStream(toStream(i), 0, false)
  }

  implicit def fromCharArrayIterator(i: Iterator[Array[Char]]): PagedStream[Char] = {
    def toStream(i: Iterator[Array[Char]]): Stream[Array[Char]] = {
      val head = if (i.hasNext) Some(i.next()) else None
      head.map(h => h #:: toStream(i)).getOrElse(Stream.empty)
    }
    PagedStream(toStream(i), 0, false)
  }

  implicit def fromStringIterable(i: Iterable[String]): PagedStream[Char] = fromStringIterator(i.iterator)

  implicit def fromCharArrayIterable(i: Iterable[Array[Char]]): PagedStream[Char] = fromCharArrayIterator(i.iterator)
}
