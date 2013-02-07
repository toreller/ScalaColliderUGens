name := "ScalaColliderUGens"

version in ThisBuild := "1.0.1"

organization in ThisBuild := "de.sciss"

description in ThisBuild := "Generates UGen source code files for ScalaCollider"

homepage in ThisBuild := Some(url("https://github.com/Sciss/ScalaColliderUGens"))

licenses in ThisBuild := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion in ThisBuild := "2.10.0"

retrieveManaged in ThisBuild := true

scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked")

// ---- build info ----
//  
// buildInfoSettings
// 
// sourceGenerators in Compile <+= buildInfo
// 
// buildInfoKeys := Seq( name, organization, version, scalaVersion, description,
//    BuildInfoKey.map( homepage ) { case (k, opt) => k -> opt.get },
//    BuildInfoKey.map( licenses ) { case (_, Seq( (lic, _) )) => "license" -> lic }
// )
// 
// buildInfoPackage := "de.sciss.synth.ugen"
