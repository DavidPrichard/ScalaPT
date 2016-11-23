name := "ScalaPT"

version      := "1.0"
sbtVersion   := "0.13.13"
scalaVersion := "2.12.0"

mainClass in Compile := Some("scalapt.MainFrame")

cancelable in Global := true // Allows you to cancel execution early with Ctrl + C

fork in run := true          // Opens project in a forked JVM, allowing sys.exit without throwing exceptions.

scalacOptions ++= Seq("-opt:_") // all optimizations

val circeVersion = "0.6.1"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect"  % "2.12.0",
  "org.typelevel" %% "cats"           % "0.8.1",
  "io.circe"      %% "circe-core"     % circeVersion,
  "io.circe"      %% "circe-generic"  % circeVersion,
  "io.circe"      %% "circe-parser"   % circeVersion
)
