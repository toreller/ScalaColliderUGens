import sbt._
import Keys._
import sbt.File
// import sbtbuildinfo.Plugin._

object Build extends sbt.Build {
  lazy val root: Project = Project(
    id = "scalacolliderugens",
    base = file("."),
    aggregate = Seq(spec, api, gen, core)
  )

  // taking inspiration from http://stackoverflow.com/questions/11509843/sbt-generate-code-using-project-defined-generator
  val ugenGenerator = TaskKey[Seq[File]]("ugen-generate", "Generate UGen class files")

  lazy val spec = Project(
    id = "scalacolliderugens-spec",
    base = file("spec"),
    settings = Project.defaultSettings /* ++ buildInfoSettings */ ++ Seq(
      // buildInfoSettings
//      sourceGenerators in Compile <+= buildInfo,
//      buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
//        BuildInfoKey.map(homepage) {
//          case (k, opt) => k -> opt.get
//        },
//        BuildInfoKey.map(licenses) {
//          case (_, Seq((lic, _))) => "license" -> lic
//        }
//      ),
//      buildInfoPackage := "de.sciss.lucre.stm"
    )
  )

  lazy val api = Project(
    id = "scalacolliderugens-api",
    base = file("api"),
//    dependencies = Seq(xml),
    settings = Project.defaultSettings ++ Seq(
//      scalaVersion := "2.10.0"
    )
  )

//  lazy val snapshots = ("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

  lazy val gen = Project(
    id = "scalacolliderugens-gen",
    base = file("gen"),
    dependencies = Seq(spec),
    settings = Project.defaultSettings ++ Seq(
//      scalaVersion := "2.9.2",  // XXX TODO
//      resolvers += snapshots,
      libraryDependencies ++= Seq(
        "com.github.scopt" %% "scopt" % "2.1.0",
//        "org.scala-refactoring" % "org.scala-refactoring_2.9.1" % "0.4.1"
        "org.scala-refactoring" % "org.scala-refactoring_2.10.0-SNAPSHOT" % "0.6.1-SNAPSHOT" from
          "https://oss.sonatype.org/content/repositories/snapshots/org/scala-refactoring/org.scala-refactoring_2.10.0-SNAPSHOT/0.6.1-SNAPSHOT/org.scala-refactoring_2.10.0-SNAPSHOT-0.6.1-20130206.063903-60.jar"
//        "org.scala-refactoring_2.10.0-SNAPSHOT-0.6.1-20130206.063903-60.jar" from snapshots
      ),
      libraryDependencies <+= scalaVersion { sv =>
        "org.scala-lang" % "scala-compiler" % sv
      }
    )
  )

  lazy val core = Project(
    id = "scalacolliderugens-core",
    base = file("core"),
    dependencies = Seq(api, gen),
    settings = Project.defaultSettings ++ Seq(
      sourceGenerators in Compile <+= (ugenGenerator in Compile),
      ugenGenerator in Compile <<=
        (sourceManaged in Compile, dependencyClasspath in Runtime in gen) map {
          (src, cp) => runUGenGenerator(src, cp.files)
        }
      )
    ) // .dependsOn(gen)

  def runUGenGenerator(outputDir: File, cp: Seq[File]): Seq[File] = {
    val scalaOutput = outputDir / "scala"

    val mainClass = "de.sciss.synth.ugen.Gen"
    val tmp       = java.io.File.createTempFile("sources", ".txt")
    val os        = new java.io.FileOutputStream(tmp)

    try {
      // NO:
      // def fork(javaHome: Option[File], options: Seq[String], workingDirectory: Option[File],
      //          env: Map[String, String], connectInput: Boolean, outputStrategy: sbt.OutputStrategy): sbt.Process
      // YES:
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
    val sources = io.Source.fromFile(tmp).getLines().map(file(_)).toList
    tmp.delete()
    sources
  }
}
