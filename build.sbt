val settings = new {
  val scalaVersion  = "3.7.4"
  val scala2Version = "2.13.18" // TODO: Switch reflection to Scala 3, if possible

  val libraryDependencies = Seq(
    "com.lihaoyi"            %% "ujson"                      % "4.4.1",
    "org.apache.commons"      % "commons-math3"              % "3.6.1",
    "org.apache.commons"      % "commons-text"               % "1.14.0",
    "org.jgrapht"             % "jgrapht-core"               % "1.5.2",
    "org.scala-lang"          % "scala-reflect"              % scala2Version,
    "org.scala-lang"          % "scala-compiler"             % scala2Version,
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    "org.scala-lang.modules" %% "scala-parser-combinators"   % "2.4.0",
    "org.scalatest"          %% "scalatest"                  % "3.2.19" % Test,
    "tools.aqua"              % "z3-turnkey"                 % "4.14.1" // Pulls in com.microsoft.z3
  )
}

// Root options
libraryDependencies ++= settings.libraryDependencies
scalacOptions ++= Seq("-deprecation", "-no-indent", "-Wunused:imports", "-Wunused:privates", "-Wunused:locals")
scalaVersion := settings.scalaVersion

// Compile options
Compile / mainClass    := Some("io.github.aaronreidsmith.AdventOfCode")
Compile / compileOrder := CompileOrder.JavaThenScala

// Run options
run / fork := true
run / javaOptions := Seq(
  "-Xms1G",
  "-Xmx8G",
  // Needed until Scala 3.8: https://www.scala-lang.org/blog/next-scala-lts.html
  "--sun-misc-unsafe-memory-access=allow",
  // 'A restricted method in java.lang.System has been called by tools.aqua.turnkey.support.TurnKey'
  "--enable-native-access=ALL-UNNAMED"
)
run / outputStrategy := Some(StdoutOutput)

// Test options
Test / envVars            := Map("IS_TEST" -> "true")
Test / fork               := true
Test / testForkedParallel := true
