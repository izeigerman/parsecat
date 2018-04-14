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
import parsecat._

trait NumericParsers extends CharacterParsers {

  lazy val long: TextParser[Long] = bigDecimalToNumeric(_.isValidLong, _.longValue(), "long")
  lazy val integer: TextParser[Int] = bigDecimalToNumeric(_.isValidInt, _.intValue(), "integer")
  lazy val short: TextParser[Short] = bigDecimalToNumeric(_.isValidShort, _.shortValue(), "short")
  lazy val byte: TextParser[Byte] = bigDecimalToNumeric(_.isValidByte, _.byteValue(), "byte")
  lazy val double: TextParser[Double] =
    bigDecimalToNumeric(_ => true, _.doubleValue(), "double") <+> infinityOrNaN(_.toDouble)
  lazy val float: TextParser[Float] =
    bigDecimalToNumeric(_ => true, _.floatValue(), "float") <+> infinityOrNaN(_.toFloat)

  lazy val bigInt: TextParser[BigInt] = {
    stringify(signedDigits).map(BigInt.apply)
  }

  lazy val bigDecimal: TextParser[BigDecimal] = {
    stringify(signedDigitsWithCommaAndExponent) map BigDecimal.apply
  }

  private lazy val digits: TextParser[List[Char]] = many1(digit)

  private lazy val signedDigits: TextParser[List[Char]] = {
    for {
      sign <- option('+', char('-') <+> char('+'))
      ds <- digits
    } yield sign :: ds
  }

  private lazy val signedDigitsWithCommaAndExponent: TextParser[List[Char]] = {
    for {
      ds <- signedDigits
      afterComma <- option(Nil, bindCons(char('.'), digits))
      exponent <- option(Nil, bindCons(oneOf(List('e', 'E')), digits))
    } yield ds ++ afterComma ++ exponent
  }

  private def infinityOrNaN[A](parse: String => A): TextParser[A] = {
    string("Infinity") <+> string("NaN") map parse
  }

  private def bigDecimalToNumeric[A <: AnyVal](isValid: BigDecimal => Boolean,
                                               toNumeric: BigDecimal => A,
                                               numericTypeName: String): TextParser[A] = {
    for {
      bd <- bigDecimal
      _ <- conversionGuard(isValid(bd), numericTypeName)
    } yield toNumeric(bd)
  }

  private def conversionGuard(b: Boolean, numericType: String): TextParser[Unit] = {
    handleConversionError(b.guard[TextParser], numericType)
  }

  private def handleConversionError[A](p: TextParser[A], numericType: String): TextParser[A] = {
    p.adaptError { case e => ParseError(e.pos, s"value is not a valid $numericType", e.debugInfo) }
  }
}
