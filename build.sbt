organization := "edu.umass.cs.iesl"

name := "paper_header"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= List(
  "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
  "IESL Public Snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public-snapshots",
  "ICM repository" at "http://maven.icm.edu.pl/artifactory/repo"
)

libraryDependencies ++= Seq(
  "pl.edu.icm.cermine" % "cermine-impl" % "1.8",
  "cc.factorie" % "factorie_2.11" % "1.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
  "com.typesafe.play" %% "play-json" % "2.5.3",

)