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

import java.io.StringReader

import cats.implicits._
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import parsecat.stream.PagedStream._

class PagedStreamSuite extends FunSuite with Matchers with PropertyChecks {

  val seed = Seed(1)

  test("PagedStream[Char].fromInputStream.success") {
    val str = stringGen(PagedStream.PageSize * 2).apply(Gen.Parameters.default, seed).get
    val offset = PagedStream.PageSize - 5
    val length = 10

    val page = PagedStream.fromReader(new StringReader(str))
    page.isSinglePage shouldBe false

    val result1 = page.slice(1, offset)
    toSlicableCharSequence(result1.right.get._1).toString shouldBe str.substring(offset, offset + 1)
    result1.right.get._2 shouldBe page

    val result2 = page.slice(length, offset)
    toSlicableCharSequence(result2.right.get._1).toString shouldBe str.substring(offset, offset + length)

    val nextPage = result2.right.get._2
    nextPage should not be page

    val result3 = nextPage.apply(PagedStream.PageSize + 1)
    result3.right.get._1 shouldBe str.charAt(PagedStream.PageSize + 1)
    result3.right.get._2 shouldBe nextPage
  }

  test("PagedStream[Char].fromReader.failure") {
    val str = stringGen(PagedStream.PageSize * 2).apply(Gen.Parameters.default, seed).get

    val page = PagedStream.fromReader(new StringReader(str))
    val nextPage = page.slice(1, PagedStream.PageSize + 1).right.get._2

    page.slice(1, PagedStream.PageSize * 2 + 1) shouldBe "unexpected end of input".asLeft
    page.apply(PagedStream.PageSize * 2 + 1) shouldBe "unexpected end of input".asLeft

    nextPage.slice(1, 0) shouldBe "offset can't be smaller than the current stream position".asLeft
    nextPage.apply(0) shouldBe "offset can't be smaller than the current stream position".asLeft
  }

  test("PagedStream[Char].fromStringIterator.success") {
    val str = stringGen(PagedStream.PageSize * 2).apply(Gen.Parameters.default, seed).get

    val page = PagedStream.fromStringIterator(str.grouped(100))
    page.isSinglePage shouldBe false

    val offset = 95
    val length = 10

    val result1 = page.slice(length, offset)
    toSlicableCharSequence(result1.right.get._1).toString shouldBe str.substring(offset, offset + length)

    val nextPage = result1.right.get._2
    nextPage should not be page

    val result3 = nextPage.apply(PagedStream.PageSize + 1)
    result3.right.get._1 shouldBe str.charAt(PagedStream.PageSize + 1)
    result3.right.get._2 should not be nextPage
  }

  test("PagedStream.CompositeSlicableSequence") {
    val seq1 = SlicedSequence(Array('t', 'e', 's', 't'), 0, 4)
    val seq2 = SlicedSequence(Array('1', '2', '3'), 0, 3)
    val compositeSequence = CompositeSlicableSequence(seq1, seq2)
    compositeSequence.length shouldBe 7
    compositeSequence.subSequence(4, 6).toString shouldBe "12"
    compositeSequence.subSequence(3, 6).toString shouldBe "t12"
    compositeSequence.subSequence(2, 4).toString shouldBe "st"
    compositeSequence.charAt(3) shouldBe 't'
    compositeSequence.charAt(4) shouldBe '1'
    toSlicableCharSequence(compositeSequence).toString shouldBe "test123"
  }

  def stringGen(length: Int): Gen[String] = Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
}
