name := "Gorillas Maps"

version := "0.1"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.8" % "test"
)

testOptions in Test ++= Seq(Tests.Argument("-l", "NonDeterministic"))
