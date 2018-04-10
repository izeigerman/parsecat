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
package parsecat

import cats._
import cats.implicits._
import ParserT._

trait Combinator {

  final def many[F[_], S, C, A](p: ParserT[F, S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, List[A]] = {
    lazy val nested: ParserT[F, S, C, List[A]] = bindCons(p, nested) <+> parserTPure(Nil)
    nested
  }

  final def many1[F[_], S, C, A](p: ParserT[F, S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, List[A]] = {
    bindCons(p, many(p))
  }

  final def skipMany[F[_], S, C, A](p: ParserT[F, S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, Unit] = {
    many(p).map(_ => ())
  }

  final def skipMany1[F[_], S, C, A](p: ParserT[F, S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, Unit] = {
    many1(p).map(_ => ())
  }

  final def ignore[F[_], S, C, A](p: ParserT[F, S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, Unit] = {
    p.map(_ => ()) <+> parserTPure(())
  }

  final def sepBy[F[_], S, C, A, B](p: ParserT[F, S, C, A],
                                    sep: ParserT[F, S, C, B])(implicit F: Monad[F]): ParserT[F, S, C, List[A]] = {
    val first = for {
      f <- optionMaybe(p)
      s <- optionMaybe(sep)
    } yield (f, s)

    val remaining = sepBy1(p, sep)

    first
      .flatMap {
        case (optFirst, optSep) =>
          optFirst
            .map(f => optSep
              .map(_ => bindCons(parserTPure[F, S, C, A](f), remaining)) // separator was found, proceed to subsequent parsing.
              .getOrElse(parserTPure[F, S, C, List[A]](List(f)))) // separator was not found, return a single parsed result.
            .getOrElse(parserTPure(Nil)) // parsing of the first occurrence failed, return empty result.
      }
    // See the comment to sepBy1(). We want to ignore the possible parsing error for the first occurrence,
    // however we have to be cautious about subsequent parsing errors.
    //
    // sepBy1(p, sep) <+> parserTPure(Nil)
  }

  final def sepBy1[F[_], S, C, A, B](p: ParserT[F, S, C, A],
                                     sep: ParserT[F, S, C, B])(implicit F: Monad[F]): ParserT[F, S, C, List[A]] = {
    lazy val nested: ParserT[F, S, C, List[A]] = bindCons(p, optionMaybe(sep).flatMap(o => o.map(_ => nested).getOrElse(parserTPure(Nil))))
    nested
    // The implementation which relies on the many() combinator swallows the original parsing error.
    // We should ignore separator parsing errors, but if the separator was parsed successfully
    // and then subsequent parsing failed - the whole operation should be considered as failure.
    //
    // bindCons(p, many(sep >> p))
  }

  final def option[F[_], S, C, A](a: A, p: ParserT[F, S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, A] = {
    p <+> parserTPure(a)
  }

  final def optionMaybe[F[_], S, C, A](p: ParserT[F, S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, Option[A]] = {
    p.map(Some(_).asInstanceOf[Option[A]]) <+> parserTPure(None.asInstanceOf[Option[A]])
  }

  final def count[F[_], S, C, A](n: Int, p: ParserT[F, S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, List[A]] = {
    if (n <= 0) {
      parserTPure(Nil)
    } else {
      (0 until n).map(_ => p).foldRight(parserTPure[F, S, C, List[A]](Nil))((x, xs) => bindCons(x, xs))
    }
  }

  final def choice[F[_], S, C, A](ps: ParserT[F, S, C, A]*)(implicit F: Monad[F]): ParserT[F, S, C, A] = {
    parserTFoldR(ps)
  }

  final def choice[F[_], S, C, A](ps: List[ParserT[F, S, C, A]])(implicit F: Monad[F]): ParserT[F, S, C, A] = {
    parserTFoldR(ps)
  }

  final def between[F[_], S, C, A, OP, CL](open: ParserT[F, S, C, OP], close: ParserT[F, S, C, CL],
                                           p: ParserT[F, S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, A] = {
    for {
      _ <- open
      a <- p
      _ <- close
    } yield a
  }

  final def bindCons[F[_], S, C, A](p: ParserT[F, S, C, A],
                                    tail: => ParserT[F, S, C, List[A]])
                                   (implicit F: Monad[F]): ParserT[F, S, C, List[A]] = {
    p.flatMap(x => tail.map(xs => x :: xs))
  }

  protected final def parserTPure[F[_], S, C, A](a: A)(implicit F: Monad[F]): ParserT[F, S, C, A] = {
    applicativeForParserT[F, S, C].pure(a)
  }

  protected final def parserTEmpty[F[_], S, C, A](implicit F: Monad[F]): ParserT[F, S, C, A] = {
    alternativeForParserT[F, S, C].empty[A]
  }

  protected final def parserTFoldR[F[_], S, C, A](ps: Seq[ParserT[F, S, C, A]])
                                                 (implicit F: Monad[F]): ParserT[F, S, C, A] = {
    ps.foldRight(parserTEmpty[F, S, C, A])((p1, p2) => p1 <+> p2)
  }
}
