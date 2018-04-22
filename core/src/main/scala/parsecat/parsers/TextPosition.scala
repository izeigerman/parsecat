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
import cats.instances.long._

final case class TextPosition(pos: Long, row: Int, col: Int) {
  def getNextPosition(str: CharSequence): TextPosition = {
    (0 until str.length()).foldLeft(this)((p, i) => p.getNextPosition(str.charAt(i)))
  }

  def getNextPosition(char: Char): TextPosition = {
    if (char == '\n') {
      TextPosition(pos + 1, row + 1, 1)
    } else {
      TextPosition(pos + 1, row, col + 1)
    }
  }
}

object TextPosition {
  implicit val showForTextPosition: Show[TextPosition] = Show.show(p => s"row ${p.row}, column ${p.col}")
  implicit val orderForTextPosition: Order[TextPosition] = Order.by(_.pos)
}
