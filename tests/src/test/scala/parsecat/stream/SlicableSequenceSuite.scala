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

import org.scalatest.{FunSuite, Matchers}

class SlicableSequenceSuite extends FunSuite with Matchers {

  test("CompositeSlicableSequence") {
    val seq1 = SlicedSequence(Array('t', 'e', 's', 't'), 0, 4)
    val seq2 = SlicedSequence(Array('1', '2', '3'), 0, 3)
    val compositeSequence = CompositeSlicableSequence(seq1, seq2)
    compositeSequence.length shouldBe 7
    compositeSequence.subSequence(4, 6).toString shouldBe "12"
    compositeSequence.subSequence(3, 6).toString shouldBe "t12"
    compositeSequence.subSequence(2, 4).toString shouldBe "st"
    compositeSequence.charAt(3) shouldBe 't'
    compositeSequence.charAt(4) shouldBe '1'
    SlicableSequence.toCharSequence(compositeSequence).toString shouldBe "test123"
  }

}
