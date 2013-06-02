/*
 *  Control.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2013 Hanns Holger Rutz. All rights reserved.
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

import collection.immutable.{IndexedSeq => IIdxSeq}

// ---------- Control ----------

object Control {
  /**
   * Note: we are not providing further convenience methods,
   * as that is the task of ControlProxyFactory...
   */
  def ir(values: ControlProxyFactory.Values, name: Option[String] = None) = apply(scalar , values.seq, name)
  def kr(values: ControlProxyFactory.Values, name: Option[String] = None) = apply(control, values.seq, name)

  // def ir(values: Float*): Control = ir(IIdxSeq(values: _*))
  // def kr(values: Float*): Control = kr(IIdxSeq(values: _*))

  // side-effect: receiving messages from clients!
  // and more importantly: control ugens created from proxies are not wired, so they would
  // be eliminated if side-effect was false!!!
  private[ugen] final class UGen(rate: Rate, numChannels: Int, override val specialIndex: Int)
    extends UGen.MultiOut("Control", rate, IIdxSeq.fill(numChannels)(rate), IIdxSeq.empty, false, true)
}

final case class Control(rate: Rate, values: IIdxSeq[Float], ctrlName: Option[String])
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = makeUGen(IIdxSeq.empty)

  protected def makeUGen(args: IIdxSeq[UGenIn]): UGenInLike = {
    val specialIndex = UGenGraph.builder.addControl(values, ctrlName)
    new Control.UGen(rate, values.size, specialIndex)
  }
}

object ControlProxy {
  private val scalarFactory  = new ControlFactory(scalar )
  private val controlFactory = new ControlFactory(control)
}
final case class ControlProxy(rate: Rate, values: IIdxSeq[Float], name: Option[String])
  extends ControlProxyLike {

  private[synth] def factory: ControlFactoryLike = rate match {
    case `scalar`   => ControlProxy.scalarFactory
    case `control`  => ControlProxy.controlFactory
    case _          => sys.error(s"Unsupported rate $rate")
  }
}

final class ControlFactory(rate: Rate) extends ControlFactoryLike {
  protected def makeUGen(numChannels: Int, specialIndex: Int): UGen = new Control.UGen(rate, numChannels, specialIndex)
}

// ---------- TrigControl ----------

object TrigControl {
  def kr(values: ControlProxyFactory.Values, name: Option[String] = None) = TrigControl(values.seq, name)

  // def kr(values: Float*): TrigControl = kr(IIdxSeq(values: _*))

  private[ugen] final class UGen(numChannels: Int, override val specialIndex: Int)
    extends UGen.MultiOut("TrigControl", control, IIdxSeq.fill(numChannels)(control), IIdxSeq.empty, false, true)
}

final case class TrigControl(values: IIdxSeq[Float], ctrlName: Option[String])
  extends UGenSource.MultiOut with ControlRated /* with IsControl */ {

  protected def makeUGens: UGenInLike = makeUGen(IIdxSeq.empty)

  protected def makeUGen(args: IIdxSeq[UGenIn]): UGenInLike = {
    val specialIndex = UGenGraph.builder.addControl(values, ctrlName)
    new TrigControl.UGen(values.size, specialIndex)
  }
}

object TrigControlProxy {
  private object factory extends ControlFactoryLike {
    protected def makeUGen(numChannels: Int, specialIndex: Int): UGen = new TrigControl.UGen(numChannels, specialIndex)
  }
}
final case class TrigControlProxy(values: IIdxSeq[Float], name: Option[String])
  extends ControlProxyLike with ControlRated {
  private[synth] def factory: ControlFactoryLike = TrigControlProxy.factory
}

// ---------- AudioControl ----------

object AudioControl {
  def ar(values: ControlProxyFactory.Values, name: Option[String] = None) = AudioControl(values.seq, name)
  // def ar(values: Float*): AudioControl                        = ar(IIdxSeq(values: _*))

  private[ugen] final class UGen(numChannels: Int, override val specialIndex: Int)
    extends UGen.MultiOut("AudioControl", audio, IIdxSeq.fill(numChannels)(audio), IIdxSeq.empty, false, true)
}

final case class AudioControl(values: IIdxSeq[Float], ctrlName: Option[String])
  extends UGenSource.MultiOut with AudioRated /* with IsControl */ {

  protected def makeUGens: UGenInLike = makeUGen(IIdxSeq.empty)

  protected def makeUGen(args: IIdxSeq[UGenIn]): UGenInLike = {
    val specialIndex = UGenGraph.builder.addControl(values, ctrlName)
    new AudioControl.UGen(values.size, specialIndex)
  }
}

object AudioControlProxy {
  private object factory extends ControlFactoryLike {
    protected def makeUGen(numChannels: Int, specialIndex: Int): UGen = new AudioControl.UGen(numChannels, specialIndex)
  }
}
final case class AudioControlProxy(values: IIdxSeq[Float], name: Option[String])
  extends ControlProxyLike with AudioRated {
  private[synth] def factory: ControlFactoryLike = AudioControlProxy.factory
}
