name := "calculus-of-constructions"

version := "0.1.0-SNAPSHOT"

scalaVersion := "3.1.2"

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
    ,
    "org.scalacheck" %% "scalacheck" % "1.16.0", // % "test", //sbt is confused by the standard directory layout?
    "com.novocode" % "junit-interface" % "0.11" % "test"
//    ,
//    "com.github.alexarchambault" %% "scalacheck-shapeless_1.15" % "1.3.0" % "test" cross CrossVersion.for3Use2_13
)

scalacOptions ++= Seq("-deprecation", "-feature")

Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-maxDiscardRatio", "20")

