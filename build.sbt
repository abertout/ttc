name := "ttc"

version := "1.0"

scalaVersion := "2.11.7"

mainClass in (Compile, packageBin) := Some("ttc.Main")

mainClass in (Compile, run) := Some("ttc.Main")

mainClass in assembly := Some("ttc.Main")

assemblyJarName in assembly := "ttc.jar"

test in assembly := {}

libraryDependencies  ++= Seq(
"org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4",
"org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4",
"org.scalatest" % "scalatest_2.11" % "3.0.0-M14" % "test",
"org.scalanlp" %% "breeze" % "0.11.2",
"org.scalanlp" %% "breeze-natives" % "0.11.2"
)
