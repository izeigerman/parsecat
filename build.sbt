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

val CatsVersion = "1.1.0"
val ScalaTestVersion = "3.0.5"

val CommonSettings = Seq(
  organization := "com.github.izeigerman",
  scalaVersion := "2.12.2",
  version := "0.1.0-SNAPSHOT",

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-Ypartial-unification"),

  parallelExecution in Test := false
)

val ParsecatSettings = CommonSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % CatsVersion,
    "org.scalatest" %% "scalatest" % ScalaTestVersion % "test->*"
  )
)

val NoPublishSettings = CommonSettings ++ Seq(
  publishArtifact := false,
  publish := {}
)

lazy val parsecatRoot = (project in file("."))
  .settings(NoPublishSettings: _*)
  .aggregate(parsecatCore, parsecatJson)

lazy val parsecatCore = (project in file("core"))
  .settings(ParsecatSettings: _*)

lazy val parsecatJson = (project in file("json"))
  .settings(ParsecatSettings: _*)
  .dependsOn(parsecatCore)

