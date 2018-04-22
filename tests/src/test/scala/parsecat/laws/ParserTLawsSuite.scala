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
package parsecat.laws

import cats._
import cats.laws.discipline._
import cats.tests.CatsSuite
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Cogen, Gen}
import parsecat._
import parsecat.parsers._
import parsecat.parsers.character._
import parsecat.parsers.TextPosition
import parsecat.stream.PagedStream

class ParserTLawsSuite extends CatsSuite {

  val seed: Seed = Seed.apply(1)
  val stringsGen: Gen[String] = Gen.alphaNumStr

  implicit def eqForTextParser[A]: Eq[TextParser[A]] = {
    val stringsGen = Gen.alphaNumStr
    val strings = stringsGen.apply(Gen.Parameters.default, seed)
    Eq.instance((x: TextParser[A], y: TextParser[A]) => {
      strings.forall(s => {
        val pos = TextPosition(0, 1, 1)
        val xr = x.runParserT(pos, s, new TextParserContext, "")
        val yr = y.runParserT(pos, s, new TextParserContext, "")
        (xr, yr) match {
          case (Right(x), Right(y)) => x.pos == y.pos && x.output == y.output
          case (x, y) => x == y
        }
      })
    })
  }

  implicit val eqForParseError: Eq[ParseError[TextPosition]] = Eq.fromUniversalEquals

  implicit val arbitraryForTextParserChar: Arbitrary[TextParser[Char]] =
    Arbitrary(Gen.oneOf(digit, space, letter, upper, lower, tab))

  implicit val arbitraryForTextParserUnit: Arbitrary[TextParser[Unit]] =
    Arbitrary(arbitraryForTextParserChar.arbitrary.map(_.map(_ => ())))

  implicit val arbitraryForParseError: Arbitrary[ParseError[TextPosition]] =
    Arbitrary(Gen.oneOf(Seq(ParseError(TextPosition(0, 1, 1), "error", "info"))))

  implicit val cogenForParseError: Cogen[ParseError[TextPosition]] = Cogen(_ => 0L)

  implicit val arbitraryForTextParserCharToChar: Arbitrary[TextParser[Char => Char]] = {
    val ff = ParserT[Id, PagedStream[Char], TextParserContext, TextPosition, Char => Char]((pos, input, context, _) => {
      ParseOutput(pos, input, context, (_: Char) => ' ').asRight
    })
//    val failure = ParserT.parserTError[Id, String, Unit, TextPosition, Char => Char](ParseError(TextPosition(0, 1, 1), "", ""))
    Arbitrary(Gen.oneOf(Seq(ff)))
  }

  implicit val isomorphismForTextParser: SemigroupalTests.Isomorphisms[TextParser] =
    SemigroupalTests.Isomorphisms.invariant

  val testName = "TextParser[Char]"
  checkAll(testName, AlternativeTests[TextParser].alternative[Char, Char, Char])
  checkAll(testName, MonadErrorTests[TextParser, ParseError[TextPosition]].monadError[Char, Char, Char])
}
