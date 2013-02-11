name := "ScalaColliderUGens"

version in ThisBuild := "1.4.0-SNAPSHOT"

organization in ThisBuild := "de.sciss"

description in ThisBuild := "UGens for ScalaCollider"

homepage in ThisBuild <<= name { n => Some(url("https://github.com/Sciss/" + n)) }

// default license - note: does not apply to the -spec sub project
// licenses in ThisBuild := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion in ThisBuild := "2.10.+"

retrieveManaged in ThisBuild := true

scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature")

initialCommands in console in ThisBuild := """import de.sciss.synth._"""

// ---- publishing ----

publishMavenStyle := true

publishTo <<= version { (v: String) =>
   Some(if (v endsWith "-SNAPSHOT")
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
   else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
   )
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra <<= name { n =>
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
