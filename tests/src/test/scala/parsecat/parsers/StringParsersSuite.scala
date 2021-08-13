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

import org.scalatest.funsuite.AnyFunSuite
import cats.implicits._
import org.scalatest.matchers.should.Matchers
import parsecat.ParseError

class StringParsersSuite extends AnyFunSuite with StringParsers with Matchers {
  test("String.string.success") {
    val result = string("test").runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    result.right.get.pos shouldBe TextPosition(4, 1, 5)
    result.right.get.output shouldBe "test"
  }

  test("String.string.failure") {
    parseText(string("test"), "123") shouldBe
      ParseError(TextPosition(0, 1, 1), "unexpected end of input", "[Parsecat] ").asLeft
    parseText(string("test"), "1234") shouldBe
      ParseError(TextPosition(0, 1, 1), "input doesn't match value 'test'", "[Parsecat] ").asLeft
  }

  test("String.satisfyMany.success") {
    val result = satisfyMany(_.isLetter).runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    result.right.get.pos shouldBe TextPosition(4, 1, 5)
    result.right.get.output.toString shouldBe "test"
  }

  test("String.satisfyMany1.success") {
    val result = satisfyMany1(_.isLetter).runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    result.right.get.pos shouldBe TextPosition(4, 1, 5)
    result.right.get.output.toString shouldBe "test"
  }

  test("String.satisfyMany1.failure") {
    val result = satisfyMany1(_.isDigit).runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    result shouldBe ParseError(TextPosition(0, 1, 1), "no characters satisfied the condition", "").asLeft
  }

  test("String.anyCharTill.failure") {
    val result = anyCharTill(_.isDigit).runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    result.right.get.pos shouldBe TextPosition(4, 1, 5)
    result.right.get.output.toString shouldBe "test"
  }

  test("String.noneOfMany.success") {
    val result1 = noneOfMany("123").runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    val result2 = noneOfMany(List('1', '2', '3')).runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    result1.right.get.pos shouldBe TextPosition(4, 1, 5)
    result1.right.get.output.toString shouldBe "test"
    result2.right.get.pos shouldBe result1.right.get.pos
    result2.right.get.output.toString shouldBe result1.right.get.output.toString
  }

  test("String.oneOfMany.success") {
    val result1 = oneOfMany("test").runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    val result2 = oneOfMany(List('t', 'e', 's', 't')).runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    result1.right.get.pos shouldBe TextPosition(4, 1, 5)
    result1.right.get.output.toString shouldBe "test"
    result2.right.get.pos shouldBe result1.right.get.pos
    result2.right.get.output.toString shouldBe result1.right.get.output.toString
  }
}
