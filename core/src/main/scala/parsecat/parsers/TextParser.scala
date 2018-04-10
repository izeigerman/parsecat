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

import scala.util.matching.Regex

trait TextParser extends Combinator {
  type TextParser[A] = ParserT.Parser[String, Unit, A]

  final def parseText[A](parser: TextParser[A], text: String): Either[ParseError, A] = {
    parser.parse(text, ())
  }

  lazy val anyChar: TextParser[Char] =
    ParserT[Id, String, Unit, Char]((pos, input, context, info) => {
      if (input.size > pos.pos) {
        val ch = input.charAt(pos.pos)
        val newPos = getNextPos(ch, pos)
        ParseOutput(newPos, input, context, ch).asRight
      } else {
        ParseError(pos, "unexpected end of input", info).asLeft
      }
    })

  final def satisfy(p: Char => Boolean): TextParser[Char] = {
    ParserT[Id, String, Unit, Char]((pos, input, context, info) => {
      if (input.size > pos.pos) {
        val ch = input.charAt(pos.pos)
        val newPos = getNextPos(ch, pos)
        if (p(ch)) {
          ParseOutput(newPos, input, context, ch).asRight
        } else {
          ParseError(pos, s"unexpected character '$ch'", info).asLeft
        }
      } else {
        ParseError(pos, "unexpected end of input", info).asLeft
      }
    })
  }

  final def regex(r: Regex): TextParser[String] = {
    ParserT[Id, String, Unit, String]((pos, input, context, info) => {
      if (input.size > pos.pos) {
        val regexMatch = r.findPrefixOf(TextParser.ShiftedString(input, pos.pos))
        regexMatch
          .map(out => ParseOutput(getNextPos(out, pos), input, context, out).asRight)
          .getOrElse(ParseError(pos, s"input doesn't match regex '$r'", info).asLeft)
      } else {
        ParseError(pos, "unexpected end of input", info).asLeft
      }
    })
  }

  final def string(s: String): TextParser[String] = {
    ParserT[Id, String, Unit, String]((pos, input, context, info) => {
      if (input.size >= (pos.pos + s.length)) {
        if (input.startsWith(s, pos.pos)) {
          ParseOutput(getNextPos(s, pos), input, context, s).asRight
        } else {
          ParseError(pos, s"input doesn't match value '$s'", info).asLeft
        }
      } else {
        ParseError(pos, "unexpected end of input", info).asLeft
      }
    })
//    stringify(s.map(char(_)).foldRight(parserTPure[Id, String, Unit, List[Char]](Nil))((x, xs) => bindCons(x, xs)))
  }

  final def char(expected: Char): TextParser[Char] = {
    satisfy(_ == expected)
  }

  final def oneOf(str: String): TextParser[Char] = {
    satisfy(c => str.contains(c))
  }

  final def oneOf(str: List[Char]): TextParser[Char] = {
    satisfy(c => str.contains(c))
  }

  final def noneOf(str: String): TextParser[Char] = {
    satisfy(c => !str.contains(c))
  }

  final def noneOf(str: List[Char]): TextParser[Char] = {
    satisfy(c => !str.contains(c))
  }

  lazy val space: TextParser[Char] = {
    char(' ')
  }

  lazy val spaces: TextParser[Unit] = {
    skipMany(space)
  }

  lazy val tab: TextParser[Char] = {
    char('\t')
  }

  lazy val upper: TextParser[Char] = {
    satisfy(_.isUpper)
  }

  lazy val lower: TextParser[Char] = {
    satisfy(_.isLower)
  }

  lazy val alphaNum: TextParser[Char] = {
    satisfy(_.isLetterOrDigit)
  }

  lazy val letter: TextParser[Char] = {
    satisfy(_.isLetter)
  }

  lazy val digit: TextParser[Char] = {
    satisfy(_.isDigit)
  }

  lazy val newline: TextParser[Char] = {
    char('\n')
  }

  lazy val crlf: TextParser[Char] = {
    char('\r') *> char('\n')
  }

  lazy val eol: TextParser[Char] = {
    newline <+> crlf
  }

  lazy val delimiters: TextParser[Unit] = {
    skipMany(space <+> tab <+> eol)
  }

  final def stringify(p: TextParser[List[Char]]): TextParser[String] = {
    p.map(_.mkString(""))
  }

  protected final def getNextPos(str: String, pos: Position): Position = {
    str.foldLeft(pos)((p, c) => getNextPos(c, p))
  }

  protected final def getNextPos(char: Char, pos: Position): Position = {
    if (char == '\n') {
      Position(pos.pos + 1, pos.row + 1, 1)
    } else {
      Position(pos.pos + 1, pos.row, pos.col + 1)
    }
  }
}

object TextParser {
  private[parsecat] final case class ShiftedString(original: String, offset: Int) extends CharSequence {

    override def length(): Int = original.length - offset

    override def subSequence(start: Int, end: Int): CharSequence =
      original.subSequence(start + offset, end + offset)

    override def charAt(index: Int): Char = original.charAt(offset + index)
  }
}
