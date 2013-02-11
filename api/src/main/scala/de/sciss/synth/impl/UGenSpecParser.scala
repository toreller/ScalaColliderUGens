package de.sciss.synth
package impl

import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut

private[synth] object UGenSpecParser {
  private def DEFAULT_VERIFY = true

  def parseAll(source: xml.InputSource): Map[String, UGenSpec] = {
    val root      = xml.XML.load(source)
    val ugenNodes = root \\ "ugen"
    ugenNodes.map( n => {
      val spec = parse(n, verify = DEFAULT_VERIFY)
      spec.name -> spec
    })(breakOut)
  }

  private val nodeAttrKeys = Set(
    "name",
    "readsbus",  "readsbuf",  "readsfft", "random", "indiv",
    "writesbus", "writesbuf", "writesfft", "sideeffect",
    "doneflag"
  )

  private implicit final class RichAttrMap(val map: Map[String, String]) extends AnyVal {
    def string (key: String): String = map.getOrElse(key, sys.error(s"Missing required attribute '${key}'"))
    def boolean(key: String, default: Boolean = false): Boolean = map.get(key).map(_.toBoolean).getOrElse(default)

//    private def getIntAttr(n: xml.Node, name: String, default: Int) =
//      (n \ ("@" + name)).headOption.map(_.text.toInt).getOrElse(default)
  }

  def parse(node: xml.Node, verify: Boolean = false): UGenSpec = {
    if (node.label != "ugen") throw new IllegalArgumentException(s"Not a 'ugen' node: ${node}")

    import UGenSpec._

    val attrs = node.attributes.asAttrMap
    if (verify) {
      val unknown = attrs.keySet -- nodeAttrKeys
      require(unknown.isEmpty, s"Unsupported ugen attributes: ${unknown.mkString(",")}")
    }

    val name          = attrs.string ("name")

    val readsBus      = attrs.boolean("readsbus")
    val readsBuffer   = attrs.boolean("readsbuf")
    val readsFFT      = attrs.boolean("readsfft")
    val random        = attrs.boolean("random")

    val writesBus     = attrs.boolean("writesbus")
    val writesBuffer  = attrs.boolean("writesbuf")
    val writesFFT     = attrs.boolean("writesfft")

    val sideEffect    = attrs.boolean("sideeffect")
    val doneFlag      = attrs.boolean("doneflag")
    val indiv         = attrs.boolean("indiv")

    var attrB         = Set.newBuilder[Attribute]
    if (readsBus)     attrB += Attribute.ReadsBus
    if (readsBuffer)  attrB += Attribute.ReadsBuffer
    if (readsFFT)     attrB += Attribute.ReadsFFT
    if (random)       attrB += Attribute.UsesRandSeed
    if (indiv)        attrB += Attribute.IsIndividual

    if (writesBus)    attrB += Attribute.WritesBus
    if (writesBuffer) attrB += Attribute.WritesBuffer
    if (writesFFT)    attrB += Attribute.WritesFFT
    if (sideEffect)   attrB += Attribute.HasSideEffect

    if (doneFlag)     attrB += Attribute.HasDoneFlag

    val indSideEffect = writesBuffer || writesFFT || writesBus
    val indIndiv      = indSideEffect || readsBus || readsBuffer || readsFFT || random

    UGenSpec(name = name, attr = attrB.result(), rates = Rates.Set(Set.empty),
      args = IIdxSeq.empty, inputs = IIdxSeq.empty, outputs = IIdxSeq.empty)
  }

//  private def booleanAttr(n: xml.Node, name: String, default: Boolean = false) =
//    (n \ ("@" + name)).headOption.map(_.text.toBoolean).getOrElse(default)
//
//  private def getIntAttr(n: xml.Node, name: String, default: Int) =
//    (n \ ("@" + name)).headOption.map(_.text.toInt).getOrElse(default)

}