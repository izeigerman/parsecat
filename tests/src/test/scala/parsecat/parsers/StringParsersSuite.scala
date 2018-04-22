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

import cats.implicits._
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import parsecat.ParseError

class StringParsersSuite extends FunSuite with StringParsers with PropertyChecks with Matchers {
  test("Text.string.success") {
    val result = string("test").runParserT(TextPosition(0, 1, 1), "test123", new TextParserContext, "")
    result.right.get.pos shouldBe TextPosition(4, 1, 5)
    result.right.get.output shouldBe "test"
  }

  test("Text.string.failure") {
    parseText(string("test"), "123") shouldBe
      ParseError(TextPosition(0, 1, 1), "unexpected end of input", "[Parsecat] ").asLeft
    parseText(string("test"), "1234") shouldBe
      ParseError(TextPosition(0, 1, 1), "input doesn't match value 'test'", "[Parsecat] ").asLeft
  }
}