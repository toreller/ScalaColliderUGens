/*
 *  UGenGraph.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth

import collection.immutable.{IndexedSeq => Vec}
import java.io.OutputStream

object UGenGraph {
  trait BuilderFactory {
    def build(graph: SynthGraph): UGenGraph
  }

  trait Builder {
    def addUGen    (ugen: UGen): Unit
    def prependUGen(ugen: UGen): Unit

    def addControl(values: Vec[Float], name: Option[String]): Int

    def visit[U](ref: AnyRef, init: => U): U
  }

  /** Installs a custom ugen graph builder on the current thread,
    * during the invocation of a closure. This method is typically
    * called from other libraries which wish to provide a graph
    * builder other than the default.
    *
    * When the method returns, the previous graph builder has automatically
    * been restored. During the execution of the `body`, calling
    * `UGenGraph.builder` will return the given `builder` argument.
    *
    * @param builder    the builder to install on the current thread
    * @param body       the body which is executed with the builder found through `UGenGraph.builder`
    * @tparam A         the result type of the body
    * @return           the result of executing the body
    */
  def use[A](builder: Builder)(body: => A): A = {
    val old = builders.get()
    builders.set(builder)
    try {
      body
    } finally {
      builders.set(old)
    }
  }

  // ---- rich ugen ----

  final case class RichUGen(ugen: UGen, inputSpecs: Traversable[(Int, Int)])

  // ---- graph builder ----

  private final val builders = new ThreadLocal[Builder] {
    override protected def initialValue = BuilderDummy
  }

  /** The current, thread local ugen graph builder instance. When called
    * outside of an explicit building process, a dummy object will be returned
    * which ignores any calls for adding ugens, but will throw an exception
    * when trying to actually expand any graph element.
    */
  def builder: Builder = builders.get

  private object BuilderDummy extends Builder {
    def build: UGenGraph = outOfContext

    def addControl(values: Vec[Float], name: Option[String]): Int = 0

    def addUGen    (ugen: UGen) = ()
    def prependUGen(ugen: UGen) = ()

    def visit[U](ref: AnyRef, init: => U): U = outOfContext

    private def outOfContext: Nothing = sys.error("Out of context")
  }
}

final case class UGenGraph(constants: Vec[Float], controlValues: Vec[Float],
                           controlNames: Vec[(String, Int)], ugens: Vec[UGenGraph.RichUGen]) {
  //   override lazy val hashCode = ... TODO: figure out how case class calculates it...
  private[synth] def write(dos: OutputStream): Unit = {
    // ---- constants ----
    writeShort(dos, constants.size)
    constants.foreach(writeFloat(dos, _))

    // ---- controls ----
    writeShort(dos, controlValues.size)
    controlValues.foreach(writeFloat(dos, _))

    writeShort(dos, controlNames.size)
    var count = 0
    controlNames.foreach { name =>
      writePascalString(dos, name._1)
      writeShort(dos, name._2)
      count += 1
    }

    writeShort(dos, ugens.size)
    ugens.foreach { ru =>
      val ugen = ru.ugen
      writePascalString(dos, ugen.name)

      dos.write(ugen.rate.id)
      writeShort(dos, ugen.numInputs)
      writeShort(dos, ugen.numOutputs)
      writeShort(dos, ugen.specialIndex)

      ru.inputSpecs.foreach { spec =>
        writeShort(dos, spec._1)
        writeShort(dos, spec._2)
      }
      ugen.outputRates.foreach(r => dos.write(r.id))
    }

    writeShort(dos, 0) // variants not supported
  }

  @inline private def writeInt(os: OutputStream, v: Int): Unit = {
    os.write((v >>> 24) & 0xFF)
    os.write((v >>> 16) & 0xFF)
    os.write((v >>>  8) & 0xFF)
    os.write((v >>>  0) & 0xFF)
  }

  @inline private def writeFloat(os: OutputStream, v: Float): Unit = writeInt(os, java.lang.Float.floatToIntBits(v))

  @inline private def writeShort(os: OutputStream, v: Int): Unit = {
    os.write((v >>> 8) & 0xFF)
    os.write((v >>> 0) & 0xFF)
  }

  @inline private def writePascalString(os: OutputStream, str: String): Unit = {
    os.write(str.length)
    os.write(str.getBytes)
  }
}