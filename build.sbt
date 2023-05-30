val settings = new {
  val scalaVersion  = "3.3.0"
  val scala2Version = "2.13.10" // TODO: Switch reflection to Scala 3, if possible

  val libraryDependencies = Seq(
    "com.lihaoyi"            %% "ujson"                      % "3.0.0",
    "org.apache.commons"      % "commons-math3"              % "3.6.1",
    "org.apache.commons"      % "commons-text"               % "1.10.0",
    "org.jgrapht"             % "jgrapht-core"               % "1.5.1",
    "org.scala-lang"          % "scala-reflect"              % scala2Version,
    "org.scala-lang"          % "scala-compiler"             % scala2Version,
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    "org.scala-lang.modules" %% "scala-parser-combinators"   % "2.2.0",
    "org.scalatest"          %% "scalatest"                  % "3.2.15" % Test
  )
}

// Root options
libraryDependencies ++= settings.libraryDependencies
scalacOptions ++= Seq("-deprecation", "-Wunused:imports", "-Wunused:privates")
scalaVersion := settings.scalaVersion

// Compile options
Compile / mainClass := Some("io.github.aaronreidsmith.AdventOfCode")

// Run options
run / fork           := true
run / javaOptions    := Seq("-Xms1G", "-Xmx8G")
run / outputStrategy := Some(StdoutOutput)

// Test options
Test / envVars            := Map("IS_TEST" -> "true")
Test / fork               := true
Test / testForkedParallel := true
Test / testOptions += {
  // Only run slow tests on CI
  sys.env.get("CI") match {
    case Some(_) => Tests.Argument(TestFrameworks.ScalaTest, "-l", "io.github.aaronreidsmith.IgnoreOnCI")
    case None    => Tests.Argument(TestFrameworks.ScalaTest, "-l", "org.scalatest.tags.Slow")
  }
}
