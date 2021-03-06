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

trait Combinators {

  /**
    * Applies the given parser zero or more times.
    */
  final def many[F[_], S, C, P, A](p: ParserT[F, S, C, P, A])(implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, List[A]] = {
    lazy val nested: ParserT[F, S, C, P, List[A]] = bindCons(p, nested) <+> parserTPure(Nil)
    nested
  }

  /**
    * Applies the given parser one or more times.
    */
  final def many1[F[_], S, C, P, A](p: ParserT[F, S, C, P, A])(implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, List[A]] = {
    bindCons(p, many(p))
  }

  /**
    * Applies parser `p` zero or more times until parser `end` succeeds. Returns the list of values
    * produced by `p`.
    */
  final def manyTill[F[_], S, C, P, A, E](p: ParserT[F, S, C, P, A], end: ParserT[F, S, C, P, E])
                                         (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, List[A]] = {
    lazy val nested: ParserT[F, S, C, P, List[A]] = end.map(_ => Nil.asInstanceOf[List[A]]) <+> bindCons(p, nested)
    nested
  }

  /**
    * Applies the given parser zero or more times ignoring its result.
    */
  final def skipMany[F[_], S, C, P, A](p: ParserT[F, S, C, P, A])(implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, Unit] = {
    many(p).map(_ => ())
  }

  /**
    * Applies the given parser one or more times ignoring its result.
    */
  final def skipMany1[F[_], S, C, P, A](p: ParserT[F, S, C, P, A])(implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, Unit] = {
    many1(p).map(_ => ())
  }

  /**
    * Applies the given parser once and ignores its result regardless of whether
    * it was a success or not.
    */
  final def optional[F[_], S, C, P, A](p: ParserT[F, S, C, P, A])(implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, Unit] = {
    p.map(_ => ()) <+> parserTPure(())
  }

  /**
    * Parses zero or more occurrences of parser `p` separated by `sep`.
    * Returns a list of values produced by parser `p`.
    */
  final def sepBy[F[_], S, C, A, P, B](p: ParserT[F, S, C, P, A],
                                       sep: ParserT[F, S, C, P, B])
                                      (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, List[A]] = {
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
              .map(_ => bindCons(parserTPure[F, S, C, P, A](f), remaining)) // separator was found, proceed to subsequent parsing.
              .getOrElse(parserTPure[F, S, C, P, List[A]](List(f)))) // separator was not found, return a single parsed result.
            .getOrElse(parserTPure(Nil)) // parsing of the first occurrence failed, return empty result.
      }
    // See the comment to sepBy1(). We want to ignore the possible parsing error for the first occurrence,
    // however we have to be cautious about subsequent parsing errors.
    //
    // sepBy1(p, sep) <+> parserTPure(Nil)
  }

  /**
    * Parses one or more occurrences of parser `p` separated by `sep`.
    * Returns a list of values produced by parser `p`.
    */
  final def sepBy1[F[_], S, C, A, P, B](p: ParserT[F, S, C, P, A],
                                        sep: ParserT[F, S, C, P, B])
                                       (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, List[A]] = {
    lazy val nested: ParserT[F, S, C, P, List[A]] = bindCons(p, optionMaybe(sep).flatMap(o => o.map(_ => nested).getOrElse(parserTPure(Nil))))
    nested
    // The implementation which relies on the many() combinator swallows the original parsing error.
    // We should ignore separator parsing errors, but if the separator was parsed successfully
    // and then subsequent parsing failed - the whole operation should be considered as failure.
    //
    // bindCons(p, many(sep >> p))
  }

  /**
    * Tries to apply parser `p` and returns the value `a` if the operation was unsuccessful.
    */
  final def option[F[_], S, C, P, A](a: A, p: ParserT[F, S, C, P, A])
                                    (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, A] = {
    p <+> parserTPure(a)
  }

  /**
    * Tries to apply parser `p`. Returns [[Some]] containing a result or [[None]] if the parsing failed.
    */
  final def optionMaybe[F[_], S, C, P, A](p: ParserT[F, S, C, P, A])
                                         (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, Option[A]] = {
    p.map(Some(_).asInstanceOf[Option[A]]) <+> parserTPure(None.asInstanceOf[Option[A]])
  }

  /**
    * Applies parser `p` exactly `n` times. Returns empty list if the `n` is less than or equals to zero,
    * otherwise returns a list of `n` values produced by parser `p`.
    */
  final def count[F[_], S, C, P, A](n: Int, p: ParserT[F, S, C, P, A])
                                   (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, List[A]] = {
    if (n <= 0) {
      parserTPure(Nil)
    } else {
      (0 until n).map(_ => p).foldRight(parserTPure[F, S, C, P, List[A]](Nil))((x, xs) => bindCons(x, xs))
    }
  }

  /**
    * Applies the given parsers in order until one of them succeeds. Returns the result of
    * a parser which succeeded.
    */
  final def choice[F[_], S, C, P, A](ps: ParserT[F, S, C, P, A]*)
                                    (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, A] = {
    parserTFoldR(ps)
  }

  /**
    * Applies the parsers in the given list in order until one of them succeeds. Returns the result of
    * a parser which succeeded.
    */
  final def choice[F[_], S, C, P, A](ps: List[ParserT[F, S, C, P, A]])
                                    (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, A] = {
    parserTFoldR(ps)
  }

  /**
    * Parses `open` then applies `p` followed by `close`. Returns result of the `p` parser.
    */
  final def between[F[_], S, C, P, A, OP, CL](open: ParserT[F, S, C, P, OP], close: ParserT[F, S, C, P, CL],
                                              p: ParserT[F, S, C, P, A])
                                             (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, A] = {
    for {
      _ <- open
      a <- p
      _ <- close
    } yield a
  }

  /**
    * Applies the given parser and returns its result while preserving the initial
    * position, input and context.
    */
  final def test[F[_], S, C, P, A](p: ParserT[F, S, C, P, A])
                                  (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, A] = {
    ParserT.apply((pos, input, context, info) => {
      F.map(p.runParserT(pos, input, context, info)) {
        case Right(ParseOutput(_, _, _, output)) => ParseOutput(pos, input, context, output).asRight
        case err => err
      }
    })
  }

  /**
    * Applies parser `then` and returns its result only if the parser `if` succeeds, otherwise the result
    * of the parser `if` is returned.
    */
  final def ifThen[F[_], S, C, P, A, B](`if`: ParserT[F, S, C, P, A], `then`: => ParserT[F, S, C, P, B])
                                       (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, B] = {
    `if` >> `then`
  }

  /**
    * Applies parser `then` and returns its result only if the parser `if` succeeds, otherwise the result
    * of the parser `else` is returned.
    */
  final def ifThenElse[F[_], S, C, P, A, B](`if`: ParserT[F, S, C, P, A], `then`: => ParserT[F, S, C, P, B],
                                            `else`: => ParserT[F, S, C, P, B])
                                           (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, B] = {
    (`if` >> `then`) <+> `else`
  }

  /**
    * A parser which returns a tuple of results produced by parsers `p1` and `p2`.
    */
  final def andThen[F[_], S, C, P, A, B](p1: ParserT[F, S, C, P, A], p2: ParserT[F, S, C, P, B])
                                        (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, (A,B)] = {
    for {
      a1 <- p1
      a2 <- p2
    } yield (a1, a2)
  }

  /**
    * Returns the result produced by the parser `p1` if it succeeds, otherwise returns
    * the result of the parser `p2` instead.
    */
  final def orElse[F[_], S, C, P, A, B >: A](p1: ParserT[F, S, C, P, A], p2: => ParserT[F, S, C, P, B])
                                            (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, B] = {
    p1.map(_.asInstanceOf[B]) <+> p2
  }

  final def bindCons[F[_], S, C, P, A](p: ParserT[F, S, C, P, A],
                                       tail: => ParserT[F, S, C, P, List[A]])
                                      (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, List[A]] = {
    p.flatMap(x => tail.map(xs => x :: xs))
  }

  protected final def parserTPure[F[_], S, C, P, A](a: A)(implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, A] = {
    applicativeForParserT[F, S, C, P].pure(a)
  }

  protected final def parserTEmpty[F[_], S, C, P, A](implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, A] = {
    alternativeForParserT[F, S, C, P].empty[A]
  }

  protected final def parserTFoldR[F[_], S, C, P, A](ps: Seq[ParserT[F, S, C, P, A]])
                                                    (implicit F: Monad[F], P0: Order[P], P1: Show[P]): ParserT[F, S, C, P, A] = {
    ps.foldRight(parserTEmpty[F, S, C, P, A])((p1, p2) => p1 <+> p2)
  }
}
