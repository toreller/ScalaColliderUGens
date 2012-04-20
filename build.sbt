name := "scalacolliderugens"

version := "0.15"

organization := "de.sciss"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
   "com.github.scopt" % "scopt_2.9.1" % "2.0.1",  // no build for Scala 2.9.2 yet
   "org.scala-lang" % "scala-compiler" % "2.9.2",
   "org.scala-refactoring" % "org.scala-refactoring.library" % "0.4.1" from
        "https://oss.sonatype.org/content/repositories/releases/org/scala-refactoring/org.scala-refactoring_2.9.1/0.4.1/org.scala-refactoring_2.9.1-0.4.1.jar"
//        "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring_2.9.1/0.3.0-SNAPSHOT/org.scala-refactoring_2.9.1-0.3.0-20110920.122544-11.jar"
)

retrieveManaged := true