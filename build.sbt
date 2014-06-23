name := """yarf"""

version := "1.0"

scalaVersion := "2.10.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.10" % "test",
  "com.typesafe.akka" % "akka-actor_2.10" % "2.3-M2",
  "com.google.guava" % "guava" % "16.0.1" ,
  "pcj" % "pcj" % "1.2"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
