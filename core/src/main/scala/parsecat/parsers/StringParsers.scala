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

import cats.Id
import cats.implicits._
import parsecat._
import parsecat.stream.{PagedStream, SlicableSequence}

trait StringParsers extends CharacterParsers {
  /**
    * The parser which succeeds for a string that equals to the given string.
    * Returns the parsed string.
    */
  final def string(s: String): TextParser[String] = {
    ParserT[Id, PagedStream[Char], TextParserContext, TextPosition, String]((pos, input, context, info) => {
      input.slice(s.length, pos.pos) match {
        case Right((actual, nextInput)) =>
          if (s.contentEquals(actual)) {
            ParseOutput(pos.getNextPosition(s), nextInput, context, s).asRight
          } else {
            context.error(pos, s"input doesn't match value '$s'", info).asLeft
          }
        case Left(e) =>
          context.error(pos, e, info).asLeft
      }
    })
    // stringify(s.map(char(_)).foldRight(parserTPure[Id, String, Unit, List[Char]](Nil))((x, xs) => bindCons(x, xs)))
  }

  def satisfyMany1(p: Char => Boolean): TextParser[CharSequence] = {
    satisfyMany(p, false)
  }

  def satisfyMany(p: Char => Boolean): TextParser[CharSequence] = {
    satisfyMany(p, true)
  }

  final def anyCharTill(end: Char => Boolean): TextParser[CharSequence] = satisfyMany(ch => !end(ch))

  final def oneOfMany(str: String): TextParser[CharSequence] = satisfyMany(c => str.contains(c))

  final def oneOfMany(str: List[Char]): TextParser[CharSequence] = satisfyMany(c => str.contains(c))

  final def oneOfMany1(str: String): TextParser[CharSequence] = satisfyMany1(c => str.contains(c))

  final def oneOfMany1(str: List[Char]): TextParser[CharSequence] = satisfyMany1(c => str.contains(c))

  final def noneOfMany(str: String): TextParser[CharSequence] = satisfyMany(c => !str.contains(c))

  final def noneOfMany(str: List[Char]): TextParser[CharSequence] = satisfyMany(c => !str.contains(c))

  final def noneOfMany1(str: String): TextParser[CharSequence] = satisfyMany1(c => !str.contains(c))

  final def noneOfMany1(str: List[Char]): TextParser[CharSequence] = satisfyMany1(c => !str.contains(c))

  /**
    * Skip zero or more spaces, tabs and end of lines in any combination.
    */
  lazy val delimiters: TextParser[Unit] = oneOfMany(List('\r', '\t', '\n', ' ')).map(_ => ())

  private def satisfyMany(p: Char => Boolean, canBeEmpty: Boolean): TextParser[CharSequence] = {
    ParserT[Id, PagedStream[Char], TextParserContext, TextPosition, CharSequence]((pos, input, context, info) => {
      input.takeWhile(pos.pos, p) match {
        case Right((sequence, nextInput)) =>
          if (sequence.length == 0 && !canBeEmpty) {
            context.error(pos, "no characters satisfied the condition", info).asLeft
          } else {
            val str = SlicableSequence.toCharSequence(sequence)
            val newPos = pos.getNextPosition(str)
            ParseOutput(newPos, nextInput, context, str).asRight
          }
        case Left(e) =>
          context.error(pos, e, info).asLeft
      }
    })
  }
}
