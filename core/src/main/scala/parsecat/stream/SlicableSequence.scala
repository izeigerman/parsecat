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

sealed trait SlicableSequence[A] {
  def length: Int
  def apply(index: Int): A
  def slice(start: Int, end: Int): SlicableSequence[A]
}

final case class SlicedSequence[A](original: Array[A], startIdx: Int, endIdx: Int) extends SlicableSequence[A] {
  override lazy val length: Int = Math.min(endIdx, original.length) - startIdx
  override def apply(index: Int): A = original(startIdx + index)
  override def slice(start: Int, end: Int): SlicableSequence[A] =
    SlicedSequence(original, start + startIdx, end + startIdx)
}

final case class CompositeSlicableSequence[A](first: SlicableSequence[A],
                                              second: SlicableSequence[A]) extends SlicableSequence[A] {
  override lazy val length: Int = first.length + second.length

  override def apply(index: Int): A = {
    if (index >= first.length) {
      second(index - first.length)
    } else {
      first(index)
    }
  }

  override def slice(start: Int, end: Int): SlicableSequence[A] = {
    if (start >= first.length) {
      second.slice(start - first.length, end - first.length)
    } else {
      if (end > first.length) {
        CompositeSlicableSequence(first.slice(start, first.length), second.slice(0, end - first.length))
      } else {
        first.slice(start, end)
      }
    }
  }
}

object SlicableSequence {

  final case class SlicableCharSequence(seq: SlicableSequence[Char]) extends CharSequence {
    override def length: Int = seq.length
    override def subSequence(start: Int, end: Int): CharSequence = SlicableCharSequence(seq.slice(start, end))
    override def charAt(index: Int): Char = seq(index)
    override def toString: String = (0 until length).map(charAt).mkString
  }

  implicit def toCharSequence(seq: SlicableSequence[Char]): CharSequence = {
    SlicableCharSequence(seq)
  }
}
