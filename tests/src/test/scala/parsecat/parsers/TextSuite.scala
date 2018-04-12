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
package parsecat.parsers

import org.scalatest.{FunSuite, Matchers}
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import parsecat.ParseError

class TextSuite extends FunSuite with Text with PropertyChecks with Matchers {
  test("Text.satisfy.success") {
    implicit val arbitraryChar = Arbitrary(Gen.alphaChar)
    forAll { (c: Char) =>
      parseText(satisfy(_.isLetter), c.toString) shouldBe c.asRight
    }
  }

  test("Text.satisfy.failure") {
    implicit val arbitraryChar = Arbitrary(Gen.numChar)
    forAll { (c: Char) =>
      parseText(satisfy(_.isLetter), c.toString) shouldBe
        ParseError(TextPosition(0, 1, 1), s"unexpected character '$c'", "[Parsecat] ").asLeft
    }
  }

  test("Text.string.success") {
    parseText(string("test"), "test1234") shouldBe "test".asRight
  }

  test("Text.string.failure") {
    parseText(string("test"), "123") shouldBe
      ParseError(TextPosition(0, 1, 1), "unexpected end of input", "[Parsecat] ").asLeft
    parseText(string("test"), "1234") shouldBe
      ParseError(TextPosition(0, 1, 1), "input doesn't match value 'test'", "[Parsecat] ").asLeft
  }
}
