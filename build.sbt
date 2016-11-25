import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

scalariformSettings

organization := "org.gnieh"

name := "easy-sync"

scalaVersion := "2.12.0"

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

ScalariformKeys.preferences := {
  ScalariformKeys.preferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
}

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.14"
