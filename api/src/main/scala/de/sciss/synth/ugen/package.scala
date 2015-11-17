/*
 *  package.scala
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
import collection.breakOut
import scala.annotation.tailrec

package object ugen {
  import language.implicitConversions

  private[synth] def stringArg(s: String): Vec[UGenIn] = {
    val bs = s.getBytes
    Constant(bs.length) +: (bs.map(Constant(_))(breakOut): Vec[UGenIn])
  }

  private[ugen] def nyquist: GE = Nyquist()

  // if the input at index `idx` has a different rate than `target`, update the
  // that input by wrapping it inside a conversion UGen
  private[ugen] def matchRate(ins: Vec[UGenIn], idx: Int, target: Rate): Vec[UGenIn] = {
    val in = ins(idx)
    if (in.rate == target) ins else {
      if (target == audio) {
        val ugenName  = if (in.rate == scalar) "DC" else "K2A"
        val upd       = UGen.SingleOut(ugenName, audio, Vector(in))
        ins.updated(idx, upd)
      } else {  // target == control | scalar
        if (in.rate == audio) {
          val ugenName  = if (target == scalar) "DC" else "A2K"
          val upd       = UGen.SingleOut(ugenName, control, Vector(in))
          ins.updated(idx, upd)
        } else ins  // can use `scalar` where `control` is required
      }
    }
  }

  // repeats `matchRate` for all arguments beginning at `idx` to the end of `ins`
  @tailrec private[ugen] def matchRateFrom(ins: Vec[UGenIn], idx: Int, target: Rate): Vec[UGenIn] =
    if (idx == ins.size) ins else {
      val ins1 = matchRate(ins, idx, target)
      matchRateFrom(ins1, idx + 1, target)
    }

  // same as matchRate but assuming that the input is a trigger signal
  private[ugen] def matchRateT(ins: Vec[UGenIn], idx: Int, target: Rate): Vec[UGenIn] = {
    val in = ins(idx)
    if (in.rate == target) ins else {
      if (target == audio) {
        val ugenName  = if (in.rate == scalar) "DC" else "T2A"
        val upd       = UGen.SingleOut(ugenName, audio, Vector(in))
        ins.updated(idx, upd)
      } else {  // target == control | scalar
        if (in.rate == audio) {
          val ugenName  = if (target == scalar) "DC" else "T2K"
          val upd       = UGen.SingleOut(ugenName, control, Vector(in))
          ins.updated(idx, upd)
        } else ins  // can use `scalar` where `control` is required
      }
    }
  }
}