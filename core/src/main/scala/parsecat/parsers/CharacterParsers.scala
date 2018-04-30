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
import parsecat.ParserT.alternativeForParserT
import parsecat._
import parsecat.stream.PagedStream

trait CharacterParsers extends Combinators {
  final def parseText[A](parser: TextParser[A], text: PagedStream[Char], info: String): Either[ParseError[TextPosition], A] = {
    parser.parse(text, new TextParserContext, TextPosition(0, 1, 1), info)
  }

  final def parseText[A](parser: TextParser[A], text: PagedStream[Char]): Either[ParseError[TextPosition], A] = {
    parser.parse(text, new TextParserContext, TextPosition(0, 1, 1))
  }

  /**
    * The parser which succeeds for any character that satisfies the given predicate.
    * Returns the parsed character.
    */
  final def satisfy(p: Char => Boolean): TextParser[Char] = {
    ParserT[Id, PagedStream[Char], TextParserContext, TextPosition, Char]((pos, input, context, info) => {
      input.apply(pos.pos) match {
        case Right((ch, nextInput)) =>
          if (p(ch)) {
            val newPos = pos.getNextPosition(ch)
            ParseOutput(newPos, nextInput, context, ch).asRight
          } else {
            context.error(pos, s"unexpected character '$ch'", info).asLeft
          }
        case Left(e) =>
          context.error(pos, e, info).asLeft
      }
    })
  }

  /**
    * Parses and returns the specified character.
    */
  final def char(expected: Char): TextParser[Char] = satisfy(_ == expected)

  /**
    * Parses and returns any character that is present in the given string.
    */
  final def oneOf(str: String): TextParser[Char] = satisfy(c => str.contains(c))

  /**
    * Parses and returns any character that is present in the given list.
    */
  final def oneOf(str: List[Char]): TextParser[Char] = satisfy(c => str.contains(c))

  /**
    * Parses and returns any character that is NOT present in the given string.
    */
  final def noneOf(str: String): TextParser[Char] = satisfy(c => !str.contains(c))

  /**
    * Parses and returns any character that is NOT present in the given list.
    */
  final def noneOf(str: List[Char]): TextParser[Char] = satisfy(c => !str.contains(c))

  /**
    * Parses and returns any character.
    */
  lazy val anyChar: TextParser[Char] = satisfy(_ => true)

  /**
    * Parses and returns a whitespace character.
    */
  lazy val space: TextParser[Char] = char(' ')

  /**
    * Skips zero or more whitespace characters.
    */
  lazy val spaces: TextParser[Unit] = skipMany(space)

  /**
    * Parses and returns a tab character.
    */
  lazy val tab: TextParser[Char] = char('\t')

  /**
    * Parses and returns an upper case letter.
    */
  lazy val upper: TextParser[Char] = satisfy(_.isUpper)

  /**
    * Parses and returns a lower case letter.
    */
  lazy val lower: TextParser[Char] = satisfy(_.isLower)

  /**
    * Parses and returns a letter or digit ('0' - '9').
    */
  lazy val alphaNum: TextParser[Char] = satisfy(_.isLetterOrDigit)

  /**
    * Parses and returns a letter.
    */
  lazy val letter: TextParser[Char] = satisfy(_.isLetter)

  /**
    * Parses and returns a digit ('0' - '9').
    */
  lazy val digit: TextParser[Char] = satisfy(_.isDigit)

  /**
    * Parses and returns a newline character.
    */
  lazy val newline: TextParser[Char] = char('\n')

  /**
    * Parses a '\r' character followed by a newline character. Returns a newline character.
    */
  lazy val crlf: TextParser[Char] = char('\r') *> char('\n')

  /**
    * The parser which succeeds if the end of line occurs. Returns a newline character.
    */
  lazy val eol: TextParser[Char] = newline <+> crlf
}
