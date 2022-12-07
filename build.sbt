val settings = new {
  val scalaVersion = "2.13.8"

  private val circeVersion = "0.14.1"

  val libraryDependencies = Seq(
    "com.google.guava"        % "guava"                    % "31.1-jre",
    "io.circe"               %% "circe-parser"             % circeVersion,
    "io.circe"               %% "circe-optics"             % circeVersion,
    "net.fornwall"            % "aoc"                      % "2019.12.442", // TODO: Don't rely on this
    "org.apache.commons"      % "commons-text"             % "1.9",
    "org.jgrapht"             % "jgrapht-core"             % "1.5.1",
    "org.scala-lang"          % "scala-reflect"            % scalaVersion,
    "org.scala-lang"          % "scala-compiler"           % scalaVersion,
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
    "org.scalatest"          %% "scalatest"                % "3.2.10" % Test
  )
}

scalaVersion := settings.scalaVersion
libraryDependencies ++= settings.libraryDependencies
scalacOptions ++= Seq("-deprecation")
