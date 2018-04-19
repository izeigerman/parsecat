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
import parsecat.stream.PagedStringStream

trait CharacterParsers extends Combinators {
  final def parseText[A](parser: TextParser[A], text: PagedStringStream, info: String): Either[ParseError[TextPosition], A] = {
    parser.parse(text, (), TextPosition(0, 1, 1), info)
  }

  final def parseText[A](parser: TextParser[A], text: PagedStringStream): Either[ParseError[TextPosition], A] = {
    parser.parse(text, (), TextPosition(0, 1, 1))
  }

  /**
    * The parser which succeeds for any character that satisfies the given predicate.
    * Returns the parsed character.
    */
  final def satisfy(p: Char => Boolean): TextParser[Char] = {
    ParserT[Id, PagedStringStream, Unit, TextPosition, Char]((pos, input, context, info) => {
      input.char() match {
        case Right((ch, nextInput)) =>
          if (p(ch)) {
            val newPos = TextPosition.getNextPos(ch, pos)
            ParseOutput(newPos, nextInput, context, ch).asRight
          } else {
            ParseError(pos, s"unexpected character '$ch'", info).asLeft
          }
        case Left(e) =>
          ParseError(pos, e, info).asLeft
      }
    })
  }

  /**
    * The parser which succeeds for a string that equals to the given string.
    * Returns the parsed string.
    */
  final def string(s: String): TextParser[String] = {
    ParserT[Id, PagedStringStream, Unit, TextPosition, String]((pos, input, context, info) => {
      input.stringOfLength(s.length) match {
        case Right((actual, nextInput)) =>
          if (actual == s) {
            ParseOutput(TextPosition.getNextPos(s, pos), nextInput, context, s).asRight
          } else {
            ParseError(pos, s"input doesn't match value '$s'", info).asLeft
          }
        case Left(e) =>
          ParseError(pos, e, info).asLeft
      }
    })
//    stringify(s.map(char(_)).foldRight(parserTPure[Id, String, Unit, List[Char]](Nil))((x, xs) => bindCons(x, xs)))
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

  /**
    * Skip zero or more spaces, tabs and end of lines in any combination.
    */
  lazy val delimiters: TextParser[Unit] = skipMany(space <+> tab <+> eol)

  /**
    * Transforms the given parser which produces a list of characters into the parser
    * which returns a string instance instead.
    */
  final def stringify(p: TextParser[List[Char]]): TextParser[String] = {
    p.map(_.mkString(""))
  }
}
