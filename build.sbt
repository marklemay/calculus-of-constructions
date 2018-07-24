name := "calculus-of-constructions"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
    "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
    "com.novocode" % "junit-interface" % "0.11",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8"
)

scalacOptions ++= Seq("-deprecation", "-feature")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxDiscardRatio", "20")

