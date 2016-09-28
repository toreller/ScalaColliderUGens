/*
 *  UGenSource.scala
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

import collection.immutable.{IndexedSeq => Vec}

object UGenSource {
  trait ZeroOut   extends UGenSource[Unit]
  trait SingleOut extends SomeOut
  trait MultiOut  extends SomeOut

  sealed trait SomeOut extends UGenSource[UGenInLike] with GE.Lazy
}

sealed trait UGenSource[U] extends Lazy.Expander[U] {
  private[synth] def makeUGen(args: Vec[UGenIn]): U

  final def name: String = productPrefix
}