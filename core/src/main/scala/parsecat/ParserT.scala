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

import cats.syntax.either._
import cats.{Alternative, Applicative, Functor, Id, Monad, MonadError}

final case class Position(pos: Int, row: Int, col: Int)

final case class ParseError(pos: Position, error: String, debugInfo: String)
  extends Error(s"${debugInfo}(pos ${pos.pos}, row ${pos.row}, column ${pos.col}): $error")

final case class ParseOutput[S, C, A](pos: Position, input: S, context: C, output: A)

final class ParserT[F[_], S, C, A](val runParserT: (Position, S, C, String) => F[Either[ParseError, ParseOutput[S, C, A]]]) {

  def parse(input: S, context: C)(implicit F: Monad[F]): F[Either[ParseError, A]] = {
    parse(input, context, "[Parsecat] ")
  }

  def parse(input: S, context: C, debugInfo: String)(implicit F: Monad[F]): F[Either[ParseError, A]] = {
    F.map(this.runParserT(Position(0, 1, 1), input, context, debugInfo)) {
      case Left(e) => e.asLeft
      case Right(o) => o.output.asRight
    }
  }

  def map[B](f: A => B)(implicit F: Monad[F]): ParserT[F, S, C, B] = {
    ParserT((pos: Position, input: S, context: C, info: String) => {
      F.map(this.runParserT(pos, input, context, info)) {
        case Right(ParseOutput(newPos, newInput, newContext, output)) =>
          ParseOutput(newPos, newInput, newContext, f(output)).asRight
        case Left(e) =>
          e.asLeft
      }
    })
  }

  def flatMap[B](f: A => ParserT[F, S, C, B])(implicit F: Monad[F]): ParserT[F, S, C, B] = {
    ParserT((pos: Position, input: S, context: C, info: String) => {
        F.flatMap(this.runParserT(pos, input, context, info)) {
          case Right(ParseOutput(newPos, newInput, newContext, output)) =>
            f(output).runParserT(newPos, newInput, newContext, info)
          case Left(e) =>
            F.pure(e.asLeft)
        }
    })
  }
}

object ParserT extends ParserTInstances {

  type Parser[S, C, A] = ParserT[Id, S, C, A]

  def apply[F[_], S, C, A](runParser: (Position, S, C, String) => F[Either[ParseError, ParseOutput[S, C, A]]]): ParserT[F, S, C, A] = {
    new ParserT[F, S, C, A](runParser)
  }

  def parserTError[F[_], S, C, A](e: ParseError)(implicit F: Monad[F]): ParserT[F, S, C, A] = {
    ParserT((_, _, _, _) => F.pure(e.asLeft))
  }

  def parserTOutput[F[_], S, C, A](o: ParseOutput[S, C, A])(implicit F: Monad[F]): ParserT[F, S, C, A] = {
    ParserT((_, _, _, _) => F.pure(o.asRight))
  }

  def liftP[F[_], S, C, A](p: F[Either[ParseError, ParseOutput[S, C, A]]]): ParserT[F, S, C, A] = {
    ParserT((_, _, _, _) => p)
  }
}

private[parsecat] trait ParserTInstances extends ParserTInstances0 {
  implicit def monadErrorForParserT[F[_], S, C](implicit F: Monad[F]): MonadError[({type λ[α] = ParserT[F, S, C, α]})#λ, ParseError] = {
    new ParserTMonadError[F, S, C] {
      override implicit val F0: Monad[F] = F
    }
  }
}

private[parsecat] sealed trait ParserTInstances0 extends ParserTInstances1 {
  implicit def monadForParserT[F[_], S, C](implicit F: Monad[F]): Monad[({type λ[α] = ParserT[F, S, C, α]})#λ] = {
    new ParserTMonad[F, S, C] {
      override implicit val F0: Monad[F] = F
    }
  }
}

private[parsecat] sealed trait ParserTInstances1 extends ParserTInstances2 {
  implicit def alternativeForParserT[F[_], S, C](implicit F: Monad[F]): Alternative[({type λ[α] = ParserT[F, S, C, α]})#λ] = {
    new ParserTAlternative[F, S, C] {
      override implicit val F0: Monad[F] = F
    }
  }
}

private[parsecat] sealed trait ParserTInstances2 extends ParserTInstances3 {
  implicit def applicativeForParserT[F[_], S, C](implicit F: Monad[F]): Applicative[({type λ[α] = ParserT[F, S, C, α]})#λ] = {
    new ParserTApplicative[F, S, C] {
      override implicit val F0: Monad[F] = F
    }
  }
}

private[parsecat] sealed trait ParserTInstances3 {
  implicit def functorForParserT[F[_], S, C](implicit F: Monad[F]): Functor[({type λ[α] = ParserT[F, S, C, α]})#λ] = {
    new ParserTFunctor[F, S, C] {
      override implicit val F0: Monad[F] = F
    }
  }
}

private[parsecat] sealed trait ParserTFunctor[F[_], S, C] extends Functor[({type λ[α] = ParserT[F, S, C, α]})#λ] {

  implicit def F0: Monad[F]

  override def map[A, B](fa: ParserT[F, S, C, A])(f: A => B): ParserT[F, S, C, B] = {
    fa.map(f)
  }
}

private[parsecat] sealed trait ParserTApplicative[F[_], S, C] extends Applicative[({type λ[α] = ParserT[F, S, C, α]})#λ] with ParserTFunctor[F, S, C] {

  override implicit def F0: Monad[F]

  override def pure[A](x: A): ParserT[F, S, C, A] = {
    ParserT((pos, input, context, _) => {
      F0.pure(ParseOutput(pos, input, context, x).asRight)
    })
  }

  override def ap[A, B](ff: ParserT[F, S, C, A => B])(fa: ParserT[F, S, C, A]): ParserT[F, S, C, B] = {
    ParserT((pos, input, context, info) => {
      F0.flatMap(ff.runParserT(pos, input, context, info)) {
        case Right(ParseOutput(newPos, newInput, newContext, f)) =>
          fa.map(f).runParserT(newPos, newInput, newContext, info)
        case Left(e) =>
          F0.pure(e.asLeft)
      }
    })
  }
}

private[parsecat] sealed trait ParserTAlternative[F[_], S, C] extends Alternative[({type λ[α] = ParserT[F, S, C, α]})#λ] with ParserTApplicative[F, S, C] {

  override implicit def F0: Monad[F]

  override def empty[A]: ParserT[F, S, C, A] = {
    ParserT((pos, _, _, info) => {
      F0.pure(ParseError(pos, "empty", info).asLeft)
    })
  }

  override def combineK[A](x: ParserT[F, S, C, A], y: ParserT[F, S, C, A]): ParserT[F, S, C, A] = {
    ParserT((pos, input, context, info) => {
      F0.flatMap(x.runParserT(pos, input, context, info)) {
        case e1 @ Left(ParseError(newPos1, _, _)) =>
          F0.map(y.runParserT(pos, input, context, info)) {
            case e2 @ Left(ParseError(newPos2, _, _)) => if (newPos1.pos >= newPos2.pos) e1 else e2
            case o => o
          }
        case o => F0.pure(o)
      }
    })
  }
}

private[parsecat] sealed trait ParserTMonad[F[_], S, C] extends Monad[({type λ[α] = ParserT[F, S, C, α]})#λ] with ParserTApplicative[F, S, C] {

  override implicit def F0: Monad[F]

  override def flatMap[A, B](fa: ParserT[F, S, C, A])(f: A => ParserT[F, S, C, B]): ParserT[F, S, C, B] = {
    fa.flatMap(f)
  }

  override def tailRecM[A, B](a: A)(f: A => ParserT[F, S, C, Either[A, B]]): ParserT[F, S, C, B] = {
    ParserT((pos, input, context, info) => {
      val init = ParseOutput(pos, input, context, a).asRight[ParseError]
      F0.tailRecM[Either[ParseError, ParseOutput[S, C, A]], Either[ParseError, ParseOutput[S, C, B]]](init) {
        case Right(ParseOutput(newPos, newInput, newContext, newA)) =>
          F0.map(f(newA).runParserT(newPos, newInput, newContext, info)) {
            case Right(ParseOutput(p, i, ctx, Left(aOutput))) =>
              ParseOutput(p, i, ctx, aOutput).asRight.asLeft
            case Right(ParseOutput(p, i, ctx, Right(bOutput))) =>
              ParseOutput(p, i, ctx, bOutput).asRight.asRight
            case Left(e) =>
              e.asLeft.asRight
          }

        case Left(e) =>
          F0.pure(e.asLeft.asRight)
      }
    })
  }
}

private[parsecat] sealed trait ParserTMonadError[F[_], S, C] extends MonadError[({type λ[α] = ParserT[F, S, C, α]})#λ, ParseError] with ParserTMonad[F, S, C] {

  override implicit def F0: Monad[F]

  override def raiseError[A](e: ParseError): ParserT[F, S, C, A] = ParserT.parserTError(e)

  override def handleErrorWith[A](fa: ParserT[F, S, C, A])(f: ParseError => ParserT[F, S, C, A]): ParserT[F, S, C, A] = {
    ParserT((pos, input, context, info) => {
      F0.flatMap(fa.runParserT(pos, input, context, info)) {
        case Left(e) =>
          f(e).runParserT(pos, input, context, info)
        case o =>
          F0.pure(o)
      }
    })
  }
}
