val scala3Version = "3.3.1"
scalacOptions ++= Seq("-deprecation", "-feature")
lazy val root = project
  .in(file("."))
  .settings(
    name := "grouse",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.9",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
  )
