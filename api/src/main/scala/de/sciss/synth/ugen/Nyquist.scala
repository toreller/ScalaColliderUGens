/*
 *  Nyquist.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

import scala.collection.immutable.{IndexedSeq => Vec}

/** A helper UGen equivalent to `SampleRate.ir * 0.5`. */
object Nyquist {
  def ir: Nyquist = new Nyquist()
}

/** A helper UGen equivalent to `SampleRate.ir * 0.5`. */
final case class Nyquist() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)

  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val sr = UGen.SingleOut("SampleRate", scalar, _args)  // `SampleRate` is only defined in `core`
    BinaryOpUGen.Times.make(sr, 0.5f)
  }
}