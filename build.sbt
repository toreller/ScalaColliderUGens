lazy val baseName       = "ScalaColliderUGens"
lazy val baseNameL      = baseName.toLowerCase

lazy val projectVersion = "1.16.4"
lazy val mimaVersion    = "1.16.0"

name := baseName

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  description        := "UGens for ScalaCollider",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion       := "2.11.8",
  crossScalaVersions := Seq("2.12.1", "2.11.8", "2.10.6"),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  initialCommands in console := """import de.sciss.synth._"""
) ++ publishSettings

// --- main dependencies ---

lazy val numbersVersion      = "0.1.3"
lazy val scalaXMLVersion     = "1.0.6"

// --- test-only dependencies ---

lazy val scalaTestVersion    = "3.0.1"

// --- gen project (not published, thus not subject to major version concerns) ---

lazy val fileUtilVersion     = "1.1.2"
lazy val scoptVersion        = "3.5.0"
lazy val refactoringVersion  = "0.8.0"

// ---

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

lazy val lgpl = Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))

lazy val spec = Project(id = s"$baseNameL-spec", base = file("spec")).
  settings(commonSettings).
  settings(
    description := "UGens XML specification files for ScalaCollider",
    autoScalaLibrary := false, // this is a pure xml containing jar
    crossPaths := false,
    licenseURL("BSD", "spec"),
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false, // there are no sources (only re-sources)
    mimaPreviousArtifacts := Set("de.sciss" % s"$baseNameL-spec" % mimaVersion)
  )

lazy val api = Project(id = s"$baseNameL-api", base = file("api")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings).
  settings(
    description := "Basic UGens API for ScalaCollider",
    licenses := lgpl,
    libraryDependencies += "de.sciss" %% "numbers" % numbersVersion,
    libraryDependencies ++= {
      val sv = scalaVersion.value
      if (!(sv startsWith "2.10")) {
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
    buildInfoPackage := "de.sciss.synth.ugen",
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-api" % mimaVersion)
  )

lazy val gen = Project(id = s"$baseNameL-gen", base = file("gen")).
  dependsOn(spec, api).
  settings(commonSettings).
  settings(
    description := "Source code generator for ScalaCollider UGens",
    licenses := lgpl,
    crossPaths := false,
    scalaVersion := "2.10.6",
    crossScalaVersions := Seq("2.10.6"),
    sbtPlugin := true,
    libraryDependencies ++= Seq(
      "com.github.scopt"      % "scopt_2.10"                         % scoptVersion,
      "org.scala-refactoring" % "org.scala-refactoring.library_2.10" % refactoringVersion,
      "de.sciss"              % "fileutil_2.10"                      % fileUtilVersion,
      "org.scalatest"         % "scalatest_2.10"                     % scalaTestVersion % "test"
    ),
    publishLocal    := {},
    publish         := {},
    publishArtifact := false,   // cf. http://stackoverflow.com/questions/8786708/
    publishTo       := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core")).
  dependsOn(api).
  settings(commonSettings).
  settings(
    description := "UGen classes for ScalaCollider",
    licenses := lgpl,
    sourceGenerators in Compile <+= ugenGenerator in Compile,
    ugenGenerator in Compile := {
      val spc   = (resourceDirectory   in Compile in spec).value
      val src   = (sourceManaged       in Compile        ).value
      val cp    = (dependencyClasspath in Runtime in gen ).value
      val st    = streams.value
      runUGenGenerator("--standard", spc, src, cp.files, st.log)
    },
    mappings in (Compile, packageSrc) ++= {
      val base  = (sourceManaged  in Compile).value
      val files = (managedSources in Compile).value
      files.map { f => (f, f.relativeTo(base).get.getPath) }
    },
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion)
  )

lazy val plugins = Project(id = s"$baseNameL-plugins", base = file("plugins")).
  dependsOn(core).
  settings(commonSettings).
  settings(
    description := "Additional third-party UGens for ScalaCollider",
    licenses := lgpl,
    sourceGenerators in Compile <+= ugenGenerator in Compile,
    ugenGenerator in Compile := {
      val spc   = (resourceDirectory   in Compile in spec).value
      val src   = (sourceManaged       in Compile        ).value
      val cp    = (dependencyClasspath in Runtime in gen ).value
      val st    = streams.value
      runUGenGenerator("--plugins", spc, src, cp.files, st.log)
    },
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-plugins" % mimaVersion)
  )

def runUGenGenerator(switch: String, specDir: File, outputDir: File, cp: Seq[File], log: Logger): Seq[File] = {
  val mainClass   = "de.sciss.synth.ugen.Gen"
  val tmp         = java.io.File.createTempFile("sources", ".txt")
  val os          = new java.io.FileOutputStream(tmp)

  log.info("Generating UGen source code...")

  try {
    val outs  = CustomOutput(os)
    val fOpt  = ForkOptions(javaHome = None, outputStrategy = Some(outs), /* runJVMOptions = Nil, */ bootJars = cp,
        workingDirectory = None, connectInput = false)
    val res: Int = Fork.scala(config = fOpt, arguments = mainClass :: switch :: "-d" :: outputDir.getAbsolutePath :: Nil)

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
