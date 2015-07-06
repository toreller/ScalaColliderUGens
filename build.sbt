lazy val baseName  = "ScalaColliderUGens"
lazy val baseNameL = baseName.toLowerCase

name := baseName

lazy val commonSettings = Seq(
  version            := "1.13.3",
  organization       := "de.sciss",
  description        := "UGens for ScalaCollider",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion       := "2.11.7",
  crossScalaVersions := Seq("2.11.7", "2.10.5"),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture"),
  initialCommands in console := """import de.sciss.synth._"""
) ++ publishSettings

lazy val numbersVersion      = "0.1.1"
lazy val scalaTestVersion    = "2.2.5"
lazy val scoptVersion        = "3.3.0"
lazy val refactoringVersion  = "0.1.0"
lazy val fileUtilVersion     = "1.1.1"
lazy val scalaXMLVersion     = "1.0.4"

lazy val root = Project(id = baseNameL, base = file(".")).
  aggregate(spec, api, gen, core, plugins).
  settings(commonSettings).
  settings(
    packagedArtifacts := Map.empty    // don't send this to Sonatype
  )

// taking inspiration from http://stackoverflow.com/questions/11509843/sbt-generate-code-using-project-defined-generator
lazy val ugenGenerator = TaskKey[Seq[File]]("ugen-generate", "Generate UGen class files")

def licenseURL(licName: String, sub: String) =
  licenses := Seq(licName -> url(s"https://raw.github.com/Sciss/$baseName/master/$sub/LICENSE"))

lazy val spec = Project(id = s"$baseNameL-spec", base = file("spec")).
  settings(commonSettings).
  settings(
    description := "UGens XML specification files for ScalaCollider",
    autoScalaLibrary := false, // this is a pure xml containing jar
    crossPaths := false,
    licenseURL("BSD", "spec"),
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources (only re-sources)
  )

lazy val api = Project(id = s"$baseNameL-api", base = file("api")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings).
  settings(
    description := "Basic UGens API for ScalaCollider",
    licenseURL("GPL v2+", "api"),
    libraryDependencies += "de.sciss" %% "numbers" % numbersVersion,
    libraryDependencies ++= {
      val sv = scalaVersion.value
      if (sv startsWith "2.11") {
        ("org.scala-lang.modules" %% "scala-xml" % scalaXMLVersion) :: Nil
      } else Nil
    },
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) {
        case (k, opt) => k -> opt.get
      },
      BuildInfoKey.map(licenses) {
        case (_, Seq((lic, _))) => "license" -> lic
      }
    ),
    buildInfoPackage := "de.sciss.synth.ugen"
  )

lazy val gen = Project(id = s"$baseNameL-gen", base = file("gen")).
  dependsOn(spec, api).
  settings(commonSettings).
  settings(
    description := "Source code generator for ScalaCollider UGens",
    licenseURL("GPL v2+", "gen"),
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt"            % scoptVersion,
      "de.sciss"         %% "scalarefactoring" % refactoringVersion,
      "de.sciss"         %% "fileutil"         % fileUtilVersion,
      "org.scalatest"    %% "scalatest"        % scalaTestVersion % "test"
    ),
    publishLocal := {},
    publish := {}
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core")).
  dependsOn(api).
  settings(commonSettings).
  settings(
    description := "UGen classes for ScalaCollider",
    licenseURL("GPL v2+", "core"),
    sourceGenerators in Compile <+= (ugenGenerator in Compile),
    ugenGenerator in Compile <<=
      (resourceDirectory   in Compile in spec,
       sourceManaged       in Compile,
       dependencyClasspath in Runtime in gen,
       streams) map {
        (spec, src, cp, st) => runUGenGenerator("--standard", spec, src, cp.files, st.log)
      }
  )

lazy val plugins = Project(id = s"$baseNameL-plugins", base = file("plugins")).
  dependsOn(core).
  settings(commonSettings).
  settings(
    description := "Additional third-party UGens for ScalaCollider",
    licenseURL("GPL v2+", "core"),
    sourceGenerators in Compile <+= (ugenGenerator in Compile),
    ugenGenerator in Compile <<=
      (resourceDirectory   in Compile in spec,
       sourceManaged       in Compile,
       dependencyClasspath in Runtime in gen,
       streams) map {
        (spec, src, cp, st) => runUGenGenerator("--plugins", spec, src, cp.files, st.log)
      }
  )

def runUGenGenerator(switch: String, specDir: File, outputDir: File, cp: Seq[File], log: Logger): Seq[File] = {
  val scalaOutput = outputDir / "scala"
  val mainClass   = "de.sciss.synth.ugen.Gen"
  val tmp         = java.io.File.createTempFile("sources", ".txt")
  val os          = new java.io.FileOutputStream(tmp)

  log.info("Generating UGen source code...")

  try {
    val outs  = CustomOutput(os)
    val fOpt  = ForkOptions(javaHome = None, outputStrategy = Some(outs), /* runJVMOptions = Nil, */ bootJars = cp,
        workingDirectory = None, connectInput = false)
    val res: Int = Fork.scala(config = fOpt, arguments = mainClass :: switch :: "-d" :: scalaOutput.getAbsolutePath :: Nil)

    if (res != 0) {
      sys.error(s"UGen class file generator failed with exit code $res")
    }
  } finally {
    os.close()
  }
  val sources = io.Source.fromFile(tmp).getLines().map(file).toList
  tmp.delete()
  sources
}

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = baseName
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
)
