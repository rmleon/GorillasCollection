name := "Gorillas Maps"

version := "0.1"

scalaVersion := "2.9.2"

organization := "gorillas"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.8" % "test"
)

testOptions in Test ++= Seq(Tests.Argument("-l", "NonDeterministic"))

pomExtra :=
<licenses>
  <license>
    <name>Gorillas Collection License</name>
    <url>https://github.com/rmleon/GorillasCollection/wiki/Gorillas-Collection-License</url>
    <distribution>repo</distribution>
  </license>
</licenses>

