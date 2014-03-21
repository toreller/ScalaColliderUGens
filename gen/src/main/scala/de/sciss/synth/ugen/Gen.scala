/*
 *  Gen.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

import xml.XML
import scopt.OptionParser
import java.io.{IOException, File}
import org.xml.sax.InputSource
import scala.collection.immutable.{IndexedSeq => Vec}

object Gen extends App {
  case class Config(resources: Boolean = false, forceOverwrite: Boolean = false, outDir: File = new File("out"),
                    inFiles: Vec[File] = Vec.empty, docs: Boolean = true)

  val parser  = new OptionParser[Config]("ScalaCollider-UGens") {
    opt[Unit]('r', "resources") text "Use resources as input"           action { (_, c) => c.copy(resources      = true ) }
    opt[Unit]('f', "force"    ) text "Force overwrite of output files"  action { (_, c) => c.copy(forceOverwrite = true ) }
    opt[File]('d', "dir"      ) text "Source output root directory"     action { (f, c) => c.copy(outDir         = f    ) }
    opt[Unit]("no-docs"       ) text "Do not include scaladoc comments" action { (_, c) => c.copy(docs           = false) }

    // help("help") text "prints this usage text"

    arg[File]("<input>...") unbounded() optional() text "List of UGen description files (XML) to process" action {
      (f, c) => c.copy(inFiles = c.inFiles :+ f)
    }
  }

  val config  = parser.parse(args, Config()) getOrElse sys.exit(1)

  implicit final class RichFile(val f: File) extends AnyVal {
    def / (sub: String) = new File(f, sub)
  }

  import config._
  val outDir1 = config.outDir / "de" / "sciss" / "synth" / "ugen"
  if (!outDir1.isDirectory) if (!outDir1.mkdirs()) throw new IOException(s"Could not create directory $outDir1")

  val synth = new ClassGenerator

  val inputs: Iterator[InputSource] = if (resources) {
    Iterator.single(xml.Source.fromInputStream(getClass.getResourceAsStream("standard-ugens.xml")))
  } else {
    inFiles.iterator.map(xml.Source.fromFile)
  }

  inputs.foreach { source =>
    val xml = XML.load(source)
    synth.performFiles(xml, outDir1, docs = docs, forceOverwrite = forceOverwrite)
  }
  // sys.exit()
}