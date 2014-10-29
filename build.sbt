name := "ScalaColliderUGens"

version            in ThisBuild := "1.11.3-SNAPSHOT"

organization       in ThisBuild := "de.sciss"

description        in ThisBuild := "UGens for ScalaCollider"

homepage           in ThisBuild := Some(url("https://github.com/Sciss/" + name.value))

scalaVersion       in ThisBuild := "2.11.2"

crossScalaVersions in ThisBuild := Seq("2.11.2", "2.10.4")

// retrieveManaged    in ThisBuild := true

scalacOptions      in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

initialCommands in console in ThisBuild := """import de.sciss.synth._"""

// ---- publishing ----

publishMavenStyle in ThisBuild := true

publishTo in ThisBuild :=
  Some(if (isSnapshot.value)
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
