scalaVersion := "2.13.7"
libraryDependencies ++= Seq(
  "org.jgrapht"             % "jgrapht-core"             % "1.5.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
  "org.scalatest"          %% "scalatest"                % "3.2.10" % Test
)
scalacOptions ++= Seq("-deprecation")
