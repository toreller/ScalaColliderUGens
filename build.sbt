name := "ScalaColliderUGens"

version         in ThisBuild := "1.7.0"

organization    in ThisBuild := "de.sciss"

description     in ThisBuild := "UGens for ScalaCollider"

homepage        in ThisBuild := Some(url("https://github.com/Sciss/" + name.value))

scalaVersion    in ThisBuild := "2.10.2"

retrieveManaged in ThisBuild := true

scalacOptions   in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature")

initialCommands in console in ThisBuild := """import de.sciss.synth._"""

// ---- publishing ----

publishMavenStyle in ThisBuild := true

publishTo in ThisBuild :=
  Some(if (version.value endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository in ThisBuild := { _ => false }

pomExtra in ThisBuild := { val n = name.value
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}
