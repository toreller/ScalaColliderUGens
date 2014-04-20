/*
 *  UGen.scala
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

import collection.immutable.{IndexedSeq => Vec}
import annotation.switch
import runtime.ScalaRunTime
import language.implicitConversions

sealed trait UGen extends Product /* with MaybeIndividual */ {
  // ---- constructor ----
  UGenGraph.builder.addUGen(this)

  def rate: Rate
  // YYY
  def numOutputs: Int
  // YYY
  //   def outputs: Vec[ UGenIn ] // YYY   XXX could be UGenProxy
  def outputRates: Vec[Rate]

  def name: String

  //   def outputRates: Seq[ Rate ]
  def inputs: Vec[UGenIn]
  def numInputs = inputs.size

  def source = this
  def specialIndex = 0

  override def toString: String = {
    val ins = inputs.mkString("(", ", ", ")")
    s"$name.${rate.methodName}$ins"
  }

  // the full UGen spec:
  // name, rate, specialIndex, inputs, outputRates
  override final def productPrefix: String = "UGen"
  final def productArity: Int = 5

  final def productElement(n: Int): Any = (n: @switch) match {
    case 0 => name
    case 1 => rate
    case 2 => specialIndex
    case 3 => inputs
    //      case 3 => inputs.map( _.ref )
    case 4 => outputRates
    case _ => throw new java.lang.IndexOutOfBoundsException(n.toString)
  }

  final def canEqual(x: Any): Boolean = x.isInstanceOf[UGen]

  override val hashCode: Int = if (isIndividual) super.hashCode() else ScalaRunTime._hashCode(this)

  override def equals(x: Any): Boolean = (this eq x.asInstanceOf[AnyRef]) || (!isIndividual && (x match {
    case u: UGen =>
      u.name == name && u.rate == rate && u.specialIndex == specialIndex && u.inputs == inputs &&
        u.outputRates == outputRates && u.canEqual(this)
    case _ => false
  }))

  def isIndividual : Boolean
  def hasSideEffect: Boolean
}

object UGen {
  /** A SingleOutUGen is a UGen which has exactly one output, and
    * hence can directly function as input to another UGen without expansion.
    */
  class SingleOut(val name: String, val rate: Rate, val inputs: Vec[UGenIn],
                  val isIndividual: Boolean = false, val hasSideEffect: Boolean = false)
    extends ugen.UGenProxy with UGen {
    final def numOutputs = 1

    final def outputRates: Vec[Rate] = rate.toIndexedSeq // Vec( rate )

    final def outputIndex = 0
  }

  object ZeroOut {
    private val outputRates: Vec[Rate] = Vec.empty
  }

  class ZeroOut(val name: String, val rate: Rate, val inputs: Vec[UGenIn], val isIndividual: Boolean = false)
    extends UGen /* with HasSideEffect */ {
    final /* override */ def numOutputs = 0

    //      final def outputs = Vec.empty
    final def outputRates: Vec[Rate] = ZeroOut.outputRates

    // Vec.empty
    final def hasSideEffect = true
  }

  /** A class for UGens with multiple outputs. */
  class MultiOut(val name: String, val rate: Rate, val outputRates: Vec[Rate], val inputs: Vec[UGenIn],
                 val isIndividual: Boolean = false, val hasSideEffect: Boolean = false)
    extends UGen with ugen.UGenInGroup {
    final def numOutputs = outputRates.size

    //      final lazy val outputs: Vec[ UGenIn ] = outputRates.zipWithIndex.map( tup => OutProxy( this, tup._2, tup._1 ))
    final def unwrap(i: Int): UGenInLike = {
      //         outputs( i % outputRates.size )
      ugen.UGenOutProxy(this, i % numOutputs) // , outputRates( i )
    }

    def outputs: Vec[UGenIn] = Vec.tabulate(numOutputs)(ch => ugen.UGenOutProxy(this, ch))

    /*
         This is important: Imagine for example `Out.ar( PanAz.ar( 4, In.ar( 0, 1 ), pos ))`.
         If `isWrapped` would report `true` here, the result would be a rewrapping into four
         `Out` UGens. Obviously we do not want this.

         This corresponds with the following check in method `MultiOutUGen: initOutputs` in sclang:
         `^if( numChannels == 1, { channels.at( 0 )}, channels )`!
       */
    //      private[synth] final def isWrapped = numOutputs != 1
    private[synth] final def unbubble: UGenInLike = if (numOutputs == 1) outputs(0) else this
  }
}

object UGenInLike {
  implicit def expand(ge: GE): UGenInLike = ge.expand
}
sealed trait UGenInLike extends GE {
  private[synth] def outputs: Vec[UGenInLike]
  private[synth] def unbubble: UGenInLike

  /** Returns the UGenInLike element of index i
    * regarding the ungrouped representation. Note
    * that for efficiency reasons this method will
    * automatically wrap the index around numElements!
    */
  private[synth] def unwrap(i: Int): UGenInLike
  private[synth] def flatOutputs: Vec[UGenIn]

  // ---- GE ----
  final private[synth] def expand: UGenInLike = this
}

/** An element that can be used as an input to a UGen.
  * This is after multi-channel-expansion, hence implementing
  * classes are SingleOutUGen, UGenOutProxy, ControlOutProxy, and Constant.
  */
sealed trait UGenIn extends UGenInLike {
  def rate: Rate

  private[synth] def outputs: Vec[UGenIn] = Vec(this)
  private[synth] final def unwrap(i: Int): UGenInLike = this

  // don't bother about the index
  private[synth] final def flatOutputs: Vec[UGenIn] = Vec(this)
  private[synth] final def unbubble   : UGenInLike  = this
}

package ugen {
  object UGenInGroup {
    private final val emptyVal = new Apply(Vec.empty)
    def empty: UGenInGroup = emptyVal
    def apply(xs: Vec[UGenInLike]): UGenInGroup = new Apply(xs)

    private final case class Apply(outputs: Vec[UGenInLike]) extends UGenInGroup {
      override def productPrefix = "UGenInGroup"

      private[synth] def numOutputs: Int = outputs.size
      private[synth] def unwrap(i: Int): UGenInLike = outputs(i % outputs.size)
      private[synth] def unbubble: UGenInLike = this

      override def toString = "UGenInGroup" + outputs.mkString("(", ",", ")")

      // ---- GE ----
      def rate: MaybeRate = MaybeRate.reduce(outputs.map(_.rate): _*)
    }
  }
  sealed trait UGenInGroup extends UGenInLike {
    private[synth] def outputs: Vec[UGenInLike]
    private[synth] def numOutputs: Int
    private[synth] final def flatOutputs: Vec[UGenIn] = outputs.flatMap(_.flatOutputs)
  }

  sealed trait UGenProxy extends UGenIn {
    def source: UGen
    def outputIndex: Int
  }

  /** A scalar constant used as an input to a UGen.
    * These constants are stored in a separate table of
    * the synth graph.
    */
  final case class Constant(value: Float) extends UGenIn with ScalarRated {
    override def toString: String = value.toString
  }

  /** A ControlOutProxy is similar to a UGenOutProxy in that it denotes
    * an output channel of a control UGen. However it refers to a control-proxy
    * instead of a real control ugen, since the proxies are synthesized into
    * actual ugens only at the end of a synth graph creation, in order to
    * clump several controls together. ControlOutProxy instance are typically
    * returned from the ControlProxyFactory class, that is, using the package
    * implicits, from calls such as "myControl".kr.
    */
  final case class ControlUGenOutProxy(source: ControlProxyLike, outputIndex: Int /*, rate: Rate */)
    extends UGenIn {

    def rate = source.rate
    override def toString = source.toString + ".\\(" + outputIndex + ")"
  }

  /** A UGenOutProxy refers to a particular output of a multi-channel UGen.
    * A sequence of these form the representation of a multi-channel-expanded
    * UGen.
    */
  final case class UGenOutProxy(source: UGen.MultiOut, outputIndex: Int /*, rate: Rate */)
    extends UGenIn with UGenProxy {

    override def toString =
      if (source.numOutputs == 1) source.toString else source.toString + ".\\(" + outputIndex + ")"

    def rate: Rate = source.outputRates(outputIndex)
  }
}