val settings = new {
  private val circeVersion = "0.14.1"

  val libraryDependencies = Seq(
    "io.circe"               %% "circe-parser"             % circeVersion,
    "io.circe"               %% "circe-optics"             % circeVersion,
    "org.jgrapht"             % "jgrapht-core"             % "1.5.1",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
    "org.scalatest"          %% "scalatest"                % "3.2.10" % Test
  )
}

scalaVersion := "2.13.7"
libraryDependencies ++= settings.libraryDependencies
scalacOptions ++= Seq("-deprecation")
