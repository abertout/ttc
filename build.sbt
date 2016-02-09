name := "ttc"

version := "1.0"

scalaVersion := "2.11.7" 

libraryDependencies  ++= Seq(
"org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4",
"org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4",
"org.scalatest" % "scalatest_2.11" % "3.0.0-M14" % "test",
"org.scalanlp" %% "breeze" % "0.11.2",
"org.scalanlp" %% "breeze-natives" % "0.11.2",
"org.scalanlp" %% "breeze-viz" % "0.11.2"
)
