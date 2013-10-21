import sbt._
import Keys._
import sbt.File
import sbtbuildinfo.Plugin._
// import com.typesafe.sbt.pgp.PgpKeys._

object Build extends sbt.Build {
  def numbersVersion = "0.1.+"

  lazy val root: Project = Project(
    id        = "scalacolliderugens",
    base      = file("."),
    aggregate = Seq(spec, api, gen, core),
    settings  = Project.defaultSettings ++ Seq(
      // publishLocal  := (),
      // publish       := (),
      // signedArtifacts := Map.empty,
      packagedArtifacts := Map.empty    // don't send this to Sonatype
      // publishSigned := ()
    )
  )

  // taking inspiration from http://stackoverflow.com/questions/11509843/sbt-generate-code-using-project-defined-generator
  lazy val ugenGenerator = TaskKey[Seq[File]]("ugen-generate", "Generate UGen class files")

  def licenseURL(licName: String, sub: String) =
    licenses <<= (name in root) { n => Seq(licName -> url("https://raw.github.com/Sciss/" + n + "/master/" + sub + "/LICENSE")) }

  lazy val spec = Project(
    id        = "scalacolliderugens-spec",
    base      = file("spec"),
    settings  = Project.defaultSettings /* ++ buildInfoSettings */ ++ Seq(
      description := "UGens XML specification files for ScalaCollider",
      autoScalaLibrary := false, // this is a pure xml containing jar
      crossPaths := false,
      licenseURL("BSD", "spec"),
      publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
      publishArtifact in (Compile, packageSrc) := false  // there are no sources (only re-sources)
    )
  )

  lazy val api = Project(
    id        = "scalacolliderugens-api",
    base      = file("api"),
    //    dependencies = Seq(xml),
    settings  = Project.defaultSettings ++ buildInfoSettings ++ Seq(
      description := "Basic UGens API for ScalaCollider",
      licenseURL("GPL v2+", "api"),
      libraryDependencies += "de.sciss" %% "numbers" % numbersVersion,
      sourceGenerators in Compile <+= buildInfo,
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
  )

  lazy val gen = Project(
    id            = "scalacolliderugens-gen",
    base          = file("gen"),
    dependencies  = Seq(spec, api),
    settings      = Project.defaultSettings ++ Seq(
      description := "Source code generator for ScalaCollider UGens",
      licenseURL("GPL v2+", "gen"),
      libraryDependencies ++= Seq(
        "com.github.scopt" %% "scopt" % "2.1.0",
        "org.scala-refactoring" % "org.scala-refactoring.library" % "0.6.1",
        "org.scalatest" %% "scalatest" % "1.9.2" % "test"
      ),
      libraryDependencies <+= scalaVersion { sv =>
        "org.scala-lang" % "scala-compiler" % sv
      },
      publishLocal := {},
      publish := {}
    )
  )

  lazy val core = Project(
    id            = "scalacolliderugens-core",
    base          = file("core"),
    dependencies  = Seq(api),
    settings      = Project.defaultSettings ++ Seq(
      description := "UGen classes for ScalaCollider",
      licenseURL("GPL v2+", "core"),
      sourceGenerators in Compile <+= (ugenGenerator in Compile),
      ugenGenerator in Compile <<=
        (resourceDirectory   in Compile in spec,
         sourceManaged       in Compile,
         dependencyClasspath in Runtime in gen,
         streams) map {
          (spec, src, cp, st) => runUGenGenerator(spec, src, cp.files, st.log)
        }
      )
    ) // .dependsOn(gen)

  def runUGenGenerator(specDir: File, outputDir: File, cp: Seq[File], log: Logger): Seq[File] = {
    val scalaOutput = outputDir / "scala"
    val mainClass   = "de.sciss.synth.ugen.Gen"
    val tmp         = java.io.File.createTempFile("sources", ".txt")
    val os          = new java.io.FileOutputStream(tmp)

    log.info("Generating UGen source code...")

    try {
      // def fork(javaHome: Option[File], jvmOptions: Seq[String], scalaJars: Iterable[File], arguments: Seq[String],
      //          workingDirectory: Option[File], connectInput: Boolean, outputStrategy: sbt.OutputStrategy): sbt.Process
      val outs  = CustomOutput(os)
      val p     = new Fork.ForkScala(mainClass).fork(javaHome = None, jvmOptions = Nil, scalaJars = cp,
        arguments = "-r" :: "-d" :: scalaOutput.getAbsolutePath :: Nil, workingDirectory = None,
        connectInput = false, outputStrategy = outs)
      val res = p.exitValue()

      if (res != 0) {
        sys.error("UGen class file generator failed with exit code " + res)
      }
    } finally {
      os.close()
    }
    val sources = io.Source.fromFile(tmp).getLines().map(file).toList
    tmp.delete()
    sources
  }
}
