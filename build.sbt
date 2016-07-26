name := "ScalaPT"

version      := "1.0"
sbtVersion   := "0.13.11"
scalaVersion := "2.11.8"

mainClass in Compile := Some("scalapt.MainFrame")

cancelable in Global := true // Allows you to cancel execution early with Ctrl + C

fork in run := true          // Opens project in a forked JVM, allowing sys.exit without throwing exceptions.


val circeVersion = "0.4.1"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect"  % "2.11.8",
  "io.circe"      %% "circe-core"     % circeVersion,
  "io.circe"      %% "circe-generic"  % circeVersion,
  "io.circe"      %% "circe-parser"   % circeVersion,
  "org.typelevel" %% "cats"           % "0.5.0"
)