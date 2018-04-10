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
package parsecat.parsers.json

import cats.implicits._
import parsecat.parsers.NumericParser

trait JsonParser extends NumericParser {

  lazy val jsParser: TextParser[JsValue] = jsValue

  lazy val jsNull: TextParser[JsNull.type] = string("null").map(_ => JsNull)

  lazy val jsInt: TextParser[JsInt] = integer.map(JsInt)

  lazy val jsLong: TextParser[JsLong] = long.map(JsLong)

  lazy val jsDouble: TextParser[JsDouble] = double.map(JsDouble)

  lazy val jsBoolean: TextParser[JsBoolean] = {
    (string("true") <+> string("false")).map(b => JsBoolean(b.toBoolean))
  }

  lazy val jsString: TextParser[JsString] = quotedString.map(JsString)

  lazy val jsArray: TextParser[JsArray] = {
    for {
      _ <- delimiters
      _ <- char('[')
      values <- sepBy(delimiters >> jsValue, char(','))
      _ <- delimiters
      _ <- char(']')
    } yield JsArray(values)
  }

  lazy val jsObject: TextParser[JsObject] = {
    val jsObjectField = for {
      _ <- delimiters
      name <- quotedString
      _ <- delimiters
      _ <- char(':')
      _ <- delimiters
      value <- jsValue
    } yield (name -> value)

    for {
      _ <- delimiters
      _ <- char('{')
      fields <- sepBy(jsObjectField, char(','))
      _ <- delimiters
      _ <- char('}')
    } yield JsObject(fields.toMap)
  }

  lazy val jsValue: TextParser[JsValue] = {
    toJsValue(jsString) <+> toJsValue(jsNull) <+> toJsValue(jsBoolean) <+> toJsValue(jsInt) <+>
      toJsValue(jsLong) <+> toJsValue(jsDouble) <+> toJsValue(jsArray) <+> toJsValue(jsObject)
  }

  private lazy val quotedString: TextParser[String] = {
    stringify(between(char('"'), char('"'), many(noneOf(List('"')))))
  }

  private def toJsValue[A <: JsValue](p: TextParser[A]): TextParser[JsValue] = {
    p.map(_.asInstanceOf[JsValue])
  }
}