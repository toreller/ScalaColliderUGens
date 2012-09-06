name := "ScalaColliderUGens"

version := "1.0.1"

organization := "de.sciss"

scalaVersion := "2.9.2"

description := "Generates UGen source code files for ScalaCollider"

homepage := Some( url( "https://github.com/Sciss/ScalaColliderUGens" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

libraryDependencies ++= Seq(
   "com.github.scopt" %% "scopt" % "2.1.0",
   "org.scala-refactoring" % "org.scala-refactoring.library" % "0.4.1"
)

libraryDependencies <+= scalaVersion { sv =>
   "org.scala-lang" % "scala-compiler" % sv
}

//   "org.scala-refactoring" % "org.scala-refactoring.library" % "0.4.1"
//    from
//        "https://oss.sonatype.org/content/repositories/releases/org/scala-refactoring/org.scala-refactoring_2.9.1/0.4.1/org.scala-refactoring_2.9.1-0.4.1.jar"
////        "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring_2.9.1/0.3.0-SNAPSHOT/org.scala-refactoring_2.9.1-0.3.0-20110920.122544-11.jar"

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// ---- build info ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq( name, organization, version, scalaVersion, description,
   BuildInfoKey.map( homepage ) { case (k, opt) => k -> opt.get },
   BuildInfoKey.map( licenses ) { case (_, Seq( (lic, _) )) => "license" -> lic }
)

buildInfoPackage := "de.sciss.synth.ugen"
