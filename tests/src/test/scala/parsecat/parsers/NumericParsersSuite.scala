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
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import parsecat.ParseError

class NumericParsersSuite extends FunSuite with NumericParsers with PropertyChecks with Matchers {

  lazy val reallyBigInt = "31273812367126716238716387123"

  testIntegralNumeric("long", long, java.lang.Long.MIN_VALUE, java.lang.Long.MIN_VALUE)
  testIntegralNumeric("integer", integer, java.lang.Integer.MIN_VALUE, java.lang.Integer.MIN_VALUE)
  testIntegralNumeric("short", short, java.lang.Short.MIN_VALUE, java.lang.Short.MIN_VALUE)
  testIntegralNumeric("byte", byte, java.lang.Byte.MIN_VALUE, java.lang.Byte.MIN_VALUE)

  def testIntegralNumeric[A: Choose](name: String, parser: TextParser[A], min: A, max: A): Unit = {
    test(s"Numeric.${name}.success") {
      forAll(Gen.choose(min, max)) { (num: A) =>
        parseText(parser, num.toString) shouldBe num.asRight
      }
    }

    test(s"Numeric.${name}.failure") {
      parseText(parser, reallyBigInt) shouldBe
        ParseError(TextPosition(29, 1, 30), s"value is not a valid ${name}", "[Parsecat] ").asLeft
    }
  }

  test("Numeric.double.success") {
    val generator: Gen[String] = Gen.oneOf("-1.23456", "+1.23456E78", "1.23456", "123456e78", "NaN", "Infinity")
    forAll(generator) { (num: String) =>
      parseText(double, num.toString) match {
        case Right(actual) if actual.isNaN => num shouldBe "NaN"
        case Right(actual) if actual.isInfinity => num shouldBe "Infinity"
        case o => o shouldBe num.toDouble.asRight
      }
    }
  }

  test("Numeric.float.success") {
    val generator: Gen[String] = Gen.oneOf("-1.23456", "+1.23456E3", "1.23456", "123456e2", "NaN", "Infinity")
    forAll(generator) { (num: String) =>
      parseText(float, num.toString) match {
        case Right(actual) if actual.isNaN => num shouldBe "NaN"
        case Right(actual) if actual.isInfinity => num shouldBe "Infinity"
        case o => o shouldBe num.toFloat.asRight
      }
    }
  }

  test("Numeric.bigint.success") {
    parseText(bigInt, reallyBigInt) shouldBe BigInt(reallyBigInt).asRight
    parseText(bigInt, "1.2345678") shouldBe BigInt(1).asRight
  }
}
