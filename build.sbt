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

val CatsVersion = "2.6.1"
val CatsTestkitVersion = "2.1.5"
val ScalaTestVersion = "3.2.9"
val DisciplineVersion = "1.1.5"

val CommonSettings = Seq(
  organization := "com.github.izeigerman",
  scalaVersion := "2.13.6",
  crossScalaVersions := Seq("2.12.14", scalaVersion.value),
  version := "0.3.0",

  organizationHomepage := Some(url("https://github.com/izeigerman")),
  homepage := Some(url("https://github.com/izeigerman/parsecat")),
  ThisBuild / licenses += ("MIT License", url("http://opensource.org/licenses/MIT")),
  developers := Developer("izeigerman", "Iaroslav Zeigerman", "", url("https://github.com/izeigerman")) :: Nil,
  scmInfo := Some(ScmInfo(
    browseUrl = url("https://github.com/izeigerman/parsecat.git"),
    connection = "scm:git:git@github.com:izeigerman/parsecat.gi"
  )),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds") ++ (
      if (scalaVersion.value.startsWith("2.12")) Seq("-Ypartial-unification") 
      else if(scalaVersion.value.startsWith("2.13")) Seq("-Wunused:imports")
      else Set.empty[String]
    )
)

val NoPublishSettings = CommonSettings ++ Seq(
  publishArtifact := false,
  publish / skip := true,
  publish := {}
)

val ParsecatSettings = CommonSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % CatsVersion
  ),

  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := (_ => false),
  publishTo := Some(
    if (version.value.trim.endsWith("SNAPSHOT")) {
      "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    } else {
      "releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    }
  )
)

val ParsecatTestsSettings = NoPublishSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-laws" % CatsVersion % "test",
    "org.typelevel" %% "cats-testkit-scalatest" % CatsTestkitVersion % "test",
    "org.scalatest" %% "scalatest" % ScalaTestVersion % "test",
    "org.typelevel" %% "discipline-core" % DisciplineVersion % "test"
  )
)

lazy val parsecatRoot = (project in file("."))
  .settings(NoPublishSettings: _*)
  .aggregate(parsecatCore, parsecatJson, parsecatTests)

lazy val parsecatCore = (project in file("core"))
  .settings(moduleName := "parsecat-core", name := "Parsecat core")
  .settings(ParsecatSettings: _*)

lazy val parsecatJson = (project in file("json"))
  .settings(moduleName := "parsecat-json", name := "Parsecat JSON")
  .settings(ParsecatSettings: _*)
  .dependsOn(parsecatCore)

lazy val parsecatTests = (project in file("tests"))
  .settings(moduleName := "parsecat-tests")
  .settings(ParsecatTestsSettings: _*)
  .dependsOn(parsecatCore, parsecatJson)

