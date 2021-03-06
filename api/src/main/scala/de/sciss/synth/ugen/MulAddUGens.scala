package de.sciss.synth
package ugen

import de.sciss.synth.UGenSource._
import Constant.{C0, C1, Cm1}

/** A UGen that multiplies an input with another signal
  * and then adds a third signal. This can be used to translate
  * an element into a different value range.
  *
  * Usually the graph element operator `madd` is used
  * instead of explicitly writing `MulAdd`.
  *
  * {{{
  * MulAdd(in, mul, add) == in.madd(mul, add) == in * mul + add
  * }}}
  *
  * ===Examples===
  *
  * {{{
  * // scale value range
  * play {
  *   // input range -1 to +1,
  *   // output range ((-1 * 100) + 300) = 200
  *   // to           ((+1 * 100) + 300) = 400
  *   val freq = SinOsc.kr(1).madd(100, 300) // or MulAdd(..., 100, 300)
  *   freq.poll(label = "freq")
  *   SinOsc.ar(freq) * 0.1
  * }
  * }}}
  *
  * @param in   the input signal to scale and offset
  * @param mul  the scaling factor, applied first to the input
  * @param add  the offset, added after applying the multiplication
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.Sum3$ Sum3]]
  * @see [[de.sciss.synth.ugen.Sum4$ Sum4]]
  */
final case class MulAdd(in: GE, mul: GE, add: GE)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector(in.expand, mul.expand, add.expand))

  def rate: MaybeRate = in.rate // XXX TODO - correct?

  private[synth] def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    import BinaryOpUGen.{Minus, Plus, Times}
    import UnaryOpUGen.Neg

    val in0  = args(0)
    val mul0 = args(1)
    val add0 = args(2)
    (mul0, add0) match {
      case (C0, _)    => add0
      case (C1, C0)   => in0
      case (C1, _)    => Plus .make1(in0, add0)
      case (Cm1, C0)  => Neg  .make1(in0)
      case (_, C0)    => Times.make1(in0, mul0)
      case (Cm1, _)   => Minus.make1(add0, in0)
      case _              =>
        if (in0.rate == `audio` ||
          (in0.rate == `control` && (mul0.rate == `control` || mul0.rate == `scalar`) &&
            (add0.rate == `control` || add0.rate == `scalar`))) {
          UGen.SingleOut(name, in0.rate, args)
        } else {
          Plus.make1(Times.make1(in0, mul0), add0)
        }
    }
  }

  override def toString = s"$in.madd($mul, $add)"
}

/** A UGen to efficiently add three signals together.
  * Usually used indirectly through `Mix`.
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.MulAdd MulAdd]]
  * @see [[de.sciss.synth.ugen.Sum4$ Sum4]]
  */
object Sum3 {
  private[ugen] def make1(args: Vec[UGenIn]): UGenIn = {
    val in0i = args(0)
    val in1i = args(1)
    val in2i = args(2)
    import BinaryOpUGen.Plus
    if (in0i == C0) {
      Plus.make1(in1i, in2i)
    } else if (in1i == C0) {
      Plus.make1(in0i, in2i)
    } else if (in2i == C0) {
      Plus.make1(in0i, in1i)
    } else {
      val rate  = in0i.rate max in1i.rate max in2i.rate
      val argsM = if (rate == audio) matchRateFrom(args, 0, audio) else args
      UGen.SingleOut("Sum3", rate, argsM)
    }
  }
}

/** A UGen to efficiently add three signals together.
  * Usually used indirectly through `Mix`.
  *
  * @param in0  first of the three inputs
  * @param in1  second of the three inputs
  * @param in2  third of the three inputs
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.MulAdd MulAdd]]
  * @see [[de.sciss.synth.ugen.Sum4$ Sum4]]
  */
final case class Sum3(in0: GE, in1: GE, in2: GE) extends UGenSource.SingleOut {
  def rate: MaybeRate = MaybeRate.max_?(in0.rate, in1.rate, in2.rate)

  protected def makeUGens: UGenInLike = unwrap(this, Vector(in0, in1, in2))

  private[synth] def makeUGen(args: Vec[UGenIn]): UGenInLike = Sum3.make1(args)
}

/** A UGen to efficiently add four signals together.
  * Usually used indirectly through `Mix`.
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.MulAdd MulAdd]]
  * @see [[de.sciss.synth.ugen.Sum3$ Sum3]]
  */
object Sum4 {
  private[ugen] def make1(args: Vec[UGenIn]): UGenIn = {
    val in0i = args(0)
    val in1i = args(1)
    val in2i = args(2)
    val in3i = args(3)

    if (in0i == C0) {
      Sum3.make1(Vec(in1i, in2i, in3i))
    } else if (in1i == C0) {
      Sum3.make1(Vec(in0i, in2i, in3i))
    } else if (in2i == C0) {
      Sum3.make1(Vec(in0i, in1i, in3i))
    } else if (in3i == C0) {
      Sum3.make1(Vec(in0i, in1i, in2i))
    } else {
      val rate  = in0i.rate max in1i.rate max in2i.rate max in3i.rate
      val argsM = if (rate == audio) matchRateFrom(args, 0, audio) else args
      UGen.SingleOut("Sum4", rate, argsM)
    }
  }
}
/** A UGen to efficiently add four signals together.
  * Usually used indirectly through `Mix`.
  *
  * @param in0  first of the four inputs
  * @param in1  second of the four inputs
  * @param in2  third of the four inputs
  * @param in3  fourth of the four inputs
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.MulAdd MulAdd]]
  * @see [[de.sciss.synth.ugen.Sum3$ Sum3]]
  */
final case class Sum4(in0: GE, in1: GE, in2: GE, in3: GE) extends UGenSource.SingleOut {
  def rate: MaybeRate = MaybeRate.max_?(in0.rate, in1.rate, in2.rate, in3.rate)

  protected def makeUGens: UGenInLike = unwrap(this, Vector(in0, in1, in2, in3))

  private[synth] def makeUGen(args: Vec[UGenIn]): UGenInLike = Sum4.make1(args)
}
