/*
 *  package.scala
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
import collection.breakOut

package object ugen {
  import language.implicitConversions

  private[ugen] def stringArg(s: String): Vec[UGenIn] = {
    val bs = s.getBytes
    Constant(bs.length) +: (bs.map(Constant(_))(breakOut): Vec[UGenIn])
  }

  private[ugen] def nyquist: GE = BinaryOpUGen.Times.make(SampleRate.ir, 0.5f)

  private[ugen] def replaceZeroesWithSilence(ins: Vec[UGenIn]): Vec[UGenIn] = {
    val numZeroes = ins.foldLeft(0)((sum, in) => in match {
      case Constant(0)  => sum + 1
      case _            => sum
    })
    if (numZeroes == 0) {
      ins
    } else {
      // WARNING: Silent has been removed now from scsynth !!!
      //         val silent = Silent.ar( numZeroes ).outputs.iterator
      //         val silent = new UGen.MultiOut( "Silent", audio, Vec.fill( numZeroes )( audio ), Vec.empty ).outputs.iterator
      val silent = new UGen.MultiOut("DC", audio, Vec(audio), Vec(Constant(0))).outputs.head
      ins.map {
        case Constant(0) => silent // .next()
        case x => x
      }
    }
  }
}