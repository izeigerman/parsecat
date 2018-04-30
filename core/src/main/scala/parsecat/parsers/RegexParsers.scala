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

import cats._
import cats.implicits._
import parsecat._
import parsecat.stream.PagedStream

import scala.util.matching.Regex

trait RegexParsers extends CharacterParsers {
  /**
    * The parser which succeeds for a string that matches the given regular expression.
    * Returns a string that matched the regular expression. Supported only for single-page streams.
    */
  final def regex(r: Regex): TextParser[String] = {
    ParserT[Id, PagedStream[Char], TextParserContext, TextPosition, String]((pos, input, context, info) => {
      if (input.isSinglePage) {
        val remainder = input.pageRemainder(pos.pos)
        val regexMatch = r.findPrefixOf(remainder)
        regexMatch
          .map(out => ParseOutput(pos.getNextPosition(out), input, context, out).asRight)
          .getOrElse(context.error(pos, s"input doesn't match regex '$r'", info).asLeft)
      } else {
        context.error(pos, "can't apply regex on a multi-page stream", info).asLeft
      }
    })
  }
}
