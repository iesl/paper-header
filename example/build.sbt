name := "test-ph"

version := "1.0"

resolvers ++= Seq(
  "IESL Public Snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/public-snapshots",
  "IESL Public Releases" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public"
)

libraryDependencies ++= Seq(
  "cc.factorie" % "factorie" % "1.1-SNAPSHOT",
  "cc.factorie.app.nlp" % "all-models" % "1.0-SNAPSHOT"
)