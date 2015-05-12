organization := "edu.umass.cs.iesl"

name := "paper_header"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "IESL Public Snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/public-snapshots",
  "IESL Public Releases" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public"
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "cc.factorie" % "factorie_2.11" % "1.2-SNAPSHOT",
  "cc.factorie.app.nlp" % "all-models" % "1.0.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

exportJars := true
