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
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import parsecat.ParseError
import parsecat.parsers.TextPosition

class JsonParsersSuite extends AnyFunSuite with JsonParsers with Matchers {

  test("Json.jsParser.success") {
    val jsonStr =
      """{
        |  "field1": "test",
        |  "field2": [
        |    1, 2, 3
        |  ],
        |  "field3": {
        |    "field4": true,
        |    "field5": null,
        |    "field6": [
        |      { "field7": 1 }, { "field8": false }
        |    ]
        |  },
        |  "field9": [],
        |  "field10": {}
        |}""".stripMargin

    val expected = JsObject(Map(
      "field1" -> JsString("test"),
      "field2" -> JsArray(List(JsInt(1), JsInt(2), JsInt(3))),
      "field3" -> JsObject(Map(
        "field4" -> JsBoolean(true),
        "field5" -> JsNull,
        "field6" -> JsArray(List(
          JsObject(Map("field7" -> JsInt(1))),
          JsObject(Map("field8" -> JsBoolean(false)))
        ))
      )),
      "field9" -> JsArray(Nil),
      "field10" -> JsObject(Map.empty)
    ))

    parseJson(jsonStr) shouldBe expected.asRight
  }

  test("Json.jsParser.failure") {
    val jsonStr =
      """{
        |  "field1": "test",
        |  "field2": [
        |    1, 2, 3
        |  ],
        |  invalid
        |}""".stripMargin
    parseJson(jsonStr) shouldBe ParseError(TextPosition(55,6,3), "unexpected character 'i'", "[JsonParser] ").asLeft
  }
}
