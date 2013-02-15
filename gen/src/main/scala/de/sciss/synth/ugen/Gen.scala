/*
 *  UGens.scala
 *  (ScalaCollider-UGens)
 *
 *  Copyright (c) 2008-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

object Gen extends App {
  var inFiles         = IndexedSeq.empty[String]
  var docs            = true
  var resources       = false
  var outDirOption    = Option.empty[String]
  var forceOverwrite  = false
  val parser          = new OptionParser( "ScalaCollider-UGens" ) {
    opt("r", "resources", "Use resources as input", action = { resources = true })
    opt("f", "force", "Force overwrite of output files", action = { forceOverwrite = true })
    opt( "d", "dir", "<directory>", "Source output root directory", (s: String) => outDirOption = Some(s))
  //         doubleOpt( "in-start", "Punch in begin (secs)", (d: Double) => punchInStart  = Some( d ))
  //         intOpt( "num-per-file", "Maximum matches per single file (default 1)", numPerFile = _ )
  //         doubleOpt( "spacing", "Minimum spacing between matches within one file (default 0.5)", minSpacing = _ )
  //         arg( "input", "UGen description file (XML) to process", (i: String) => xmlPath = i )
    arglistOpt( "inputs...", "List of UGen description files (XML) to process", action = { inFiles +:= _ })

    opt("no-docs", "Do not include scaladoc comments", action = { docs = false })
  }

  if (!parser.parse(args)) sys.exit(1)

  def file(path: String) = new File(path)

  implicit final class RichFile(val f: File) extends AnyRef {
    def /(sub: String) = new File(f, sub)
  }

  val outDir = file(outDirOption.getOrElse("out")) / "de" / "sciss" / "synth" / "ugen"
  if (!outDir.isDirectory) if (!outDir.mkdirs()) throw new IOException(s"Could not create directory ${outDir}")

  val synth = new ClassGenerator

  val inputs: Iterator[InputSource] = if (resources) {
    Iterator.single(xml.Source.fromInputStream(getClass.getResourceAsStream("standard-ugens.xml")))
  } else {
    inFiles.iterator.map(xml.Source.fromFile _)
  }

  inputs.foreach { source =>
    val xml = XML.load(source)
    synth.performFiles(xml, outDir, docs = docs, forceOverwrite = forceOverwrite)
  }
  sys.exit(0)
}