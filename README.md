# Parsecat

[![Build Status](https://travis-ci.org/izeigerman/parsecat.svg?branch=master)](https://travis-ci.org/izeigerman/parsecat)

**Parsecat** is a lightweight, pure-functional parser monad transformer and combinator library, which supports both applicative and monadic styles of parsing.

## Usage
Supports Scala 2.11 and 2.12.
```scala
libraryDependencies += "com.github.izeigerman" %% "parsecat-core" % "0.1"
// to include JSON parsers
libraryDependencies += "com.github.izeigerman" %% "parsecat-json" % "0.1"
```

### Text parser
The text parser is just a type alias for a specialized `ParsetT`:
```scala
type TextParser[A] = ParserT[Id, String, Unit, TextPosition, A]
```
It supports a 2-dimensional position tracking and has a variety of string and character implementations.

#### Character parsers
Before starting to use the parser the following imports are required:
```scala
scala> import cats.implicits._
import cats.implicits._

scala> import parsecat.parsers.text._
import parsecat.parsers.text._
```
Let's parse the following string:
```scala
scala> val str = "Hello World"
str: String = Hello World
```
A parser for this string can be defined in a couple of ways. I.e.:
```scala
scala> val parserA = andThen(string("Hello") <* space, string("World"))
parserA: parsecat.ParserT[cats.Id,String,Unit,parsecat.parsers.TextPosition,(String, String)] = parsecat.ParserT@509ec4f6
```
The parser above relies on applicative properties of `ParserT` and uses the `<*` operator to parse both: string "Hello" and a whitespace, but then keeps only a result from the first parser. `andThen` - is one of many combinator functions that are included into this library. The full list can be found [here](https://github.com/izeigerman/parsecat/blob/master/core/src/main/scala/parsecat/Combinators.scala). The result of applying of this parser to the test string is the following:
```scala
scala> parseText(parserA, str)
res1: Either[parsecat.ParseError[parsecat.parsers.TextPosition],(String, String)] = Right((Hello,World))
```
There is also a different style of combining parsers together - a monadic one. The exactly same parser can be defined differently:
```scala
scala> val parserM = andThen(string("Hello"), space >> string("World"))
parserM: parsecat.ParserT[cats.Id,String,Unit,parsecat.parsers.TextPosition,(String, String)] = parsecat.ParserT@1b718371
```
This time we rely on monadic properties of `ParserT` and use the `>>` operator - a special binding operator which discards the result of the first action. The result produced by this parser is completely the same:
```scala
scala> parseText(parserM, str)
res2: Either[parsecat.ParseError[parsecat.parsers.TextPosition],(String, String)] = Right((Hello,World))
```
When the parsing is unsuccessful the error will contain a very detailed information about what went wrong:
```scala
scala> val str = """
     | Hello
     | World
     | """.stripMargin
str: String =
"
Hello
World
"


scala> val parser = andThen(eol >> string("Hello"), eol >> regex("o.+d".r))
parser: parsecat.ParserT[cats.Id,String,Unit,parsecat.parsers.TextPosition,(String, String)] = parsecat.ParserT@2a030f21


scala> parseText(parser, str)
res4: Either[parsecat.ParseError[parsecat.parsers.TextPosition],(String, String)] = Left(parsecat.ParseError: [Parsecat] (row 3, column 1): input doesn't match regex 'o.+d')
```
Here are some other parser application examples:
```scala
scala> parseText(many(char('a')), "aabb")
res5: Either[parsecat.ParseError[parsecat.parsers.TextPosition],List[Char]] = Right(List(a, a))
```
```scala
scala> parseText(manyTill(anyChar, char('b')), "aaab")
res6: Either[parsecat.ParseError[parsecat.parsers.TextPosition],List[Char]] = Right(List(a, a, a))
```
```scala
scala> parseText(char('a') <+> char('b'), "baba")
res7: Either[parsecat.ParseError[parsecat.parsers.TextPosition],Char] = Right(b)
```
Note: in the last example we used the `<+>` operator. This is a monoid associative operator or a sum (contrary to a product provided by the applicative functor). It first applies a parser to its left and if the parsing is unsuccessful, the parser on the right side of the expression will be applied instead. The same can be expressed with a help of the `orElse` combinator:
```scala
scala> parseText(orElse(char('a'), char('b')), "baba")
res9: Either[parsecat.ParseError[parsecat.parsers.TextPosition],Char] = Right(b)
```

#### Numeric parsers
Numeric parsers - are extension to character parsers, which introduce parsing support for numeric literals. The following import is required:
```scala
scala> import parsecat.parsers.numeric._
import parsecat.parsers.numeric._
```
Examples:
```scala
scala> parseText(integer, "1234567")
res10: Either[parsecat.ParseError[parsecat.parsers.TextPosition],Int] = Right(1234567)


scala> parseText(double, "1.2345E67")
res11: Either[parsecat.ParseError[parsecat.parsers.TextPosition],Double] = Right(1.2345E67)
```
All common numeric types are supported: `byte`, `short`, `int`, `long`, `float`, `double`, `bigInt` and `bigDecimal`.

#### JSON parser
The JSON parser was added as a reference implementation and a good example of expressive power of parser combinators. The entire implementation is less than 60 LOC and could've been a part of this README, but instead you may find it here: [https://github.com/izeigerman/parsecat/blob/master/json/src/main/scala/parsecat/parsers/json/JsonParsers.scala](https://github.com/izeigerman/parsecat/blob/master/json/src/main/scala/parsecat/parsers/json/JsonParsers.scala).
```scala
scala> :paste
// Entering paste mode (ctrl-D to finish)

import parsecat.parsers.json._

val jsonStr =
  """{
    |  "field1": "test",
    |  "field2": [
    |    1, 2, 3
    |  ],
    |  "field3": {
    |    "field4": true,
    |    "field5": null,
    |    "field6": [
    |      { "field7": 1.234 }, { "field8": false }
    |    ]
    |  }
    |}""".stripMargin


parseJson(jsonStr)


// Exiting paste mode, now interpreting.


res12: Either[parsecat.ParseError[parsecat.parsers.TextPosition],parsecat.parsers.json.JsValue] = Right(JsObject(Map(field1 -> JsString(test), field2 -> JsArray(List(JsInt(1), JsInt(2), JsInt(3))), field3 -> JsObject(Map(field4 -> JsBoolean(true), field5 -> JsNull, field6 -> JsArray(List(JsObject(Map(field7 -> JsDouble(1.234))), JsObject(Map(field8 -> JsBoolean(false))))))))))
```
