package de.sciss.synth
package ugen

object ParserTest extends App {
  val is = getClass.getResourceAsStream("standard-ugens.xml")
  try {
    val doc   = xml.XML.load(is)
    val cusp  = (doc \\ "ugen").find(_.attributes.asAttrMap("name") == "CuspN").get
    UGenSpec.parse(cusp, docs = true)
  } finally {
    is.close()
  }
}