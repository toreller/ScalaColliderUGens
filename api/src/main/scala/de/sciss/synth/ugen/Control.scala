/*
 *  Control.scala
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
package ugen

import collection.immutable.{IndexedSeq => Vec}

// ---------- Control ----------

object Control {
  /**
   * Note: we are not providing further convenience methods,
   * as that is the task of ControlProxyFactory...
   */
  def ir(values: ControlValues, name: Option[String] = None) = apply(scalar , values.seq, name)
  def kr(values: ControlValues, name: Option[String] = None) = apply(control, values.seq, name)

  // def ir(values: Float*): Control = ir(Vec(values: _*))
  // def kr(values: Float*): Control = kr(Vec(values: _*))

  // side-effect: receiving messages from clients!
  // and more importantly: control ugens created from proxies are not wired, so they would
  // be eliminated if side-effect was false!!!
  private[ugen] final class UGen(rate: Rate, numChannels: Int, override val specialIndex: Int)
    extends UGen.MultiOut("Control", rate, Vector.fill(numChannels)(rate), Vector.empty, false, true)
}

final case class Control(rate: Rate, values: Vec[Float], ctrlName: Option[String])
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val specialIndex = UGenGraph.builder.addControl(values, ctrlName)
    new Control.UGen(rate, values.size, specialIndex)
  }
}

object ControlProxy {
  private val scalarFactory  = new ControlFactory(scalar )
  private val controlFactory = new ControlFactory(control)
}
final case class ControlProxy(rate: Rate, values: Vec[Float], name: Option[String])
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
  def kr(values: ControlValues, name: Option[String] = None) = TrigControl(values.seq, name)

  // def kr(values: Float*): TrigControl = kr(Vec(values: _*))

  private[ugen] final class UGen(numChannels: Int, override val specialIndex: Int)
    extends UGen.MultiOut("TrigControl", control, Vector.fill(numChannels)(control), Vector.empty, false, true)
}

final case class TrigControl(values: Vec[Float], ctrlName: Option[String])
  extends UGenSource.MultiOut with ControlRated /* with IsControl */ {

  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val specialIndex = UGenGraph.builder.addControl(values, ctrlName)
    new TrigControl.UGen(values.size, specialIndex)
  }
}

object TrigControlProxy {
  private object factory extends ControlFactoryLike {
    protected def makeUGen(numChannels: Int, specialIndex: Int): UGen = new TrigControl.UGen(numChannels, specialIndex)
  }
}
final case class TrigControlProxy(values: Vec[Float], name: Option[String])
  extends ControlProxyLike with ControlRated {
  private[synth] def factory: ControlFactoryLike = TrigControlProxy.factory
}

// ---------- AudioControl ----------

object AudioControl {
  def ar(values: ControlValues, name: Option[String] = None) = AudioControl(values.seq, name)
  // def ar(values: Float*): AudioControl                        = ar(Vec(values: _*))

  private[ugen] final class UGen(numChannels: Int, override val specialIndex: Int)
    extends UGen.MultiOut("AudioControl", audio, Vector.fill(numChannels)(audio), Vector.empty, false, true)
}

final case class AudioControl(values: Vec[Float], ctrlName: Option[String])
  extends UGenSource.MultiOut with AudioRated /* with IsControl */ {

  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val specialIndex = UGenGraph.builder.addControl(values, ctrlName)
    new AudioControl.UGen(values.size, specialIndex)
  }
}

object AudioControlProxy {
  private object factory extends ControlFactoryLike {
    protected def makeUGen(numChannels: Int, specialIndex: Int): UGen = new AudioControl.UGen(numChannels, specialIndex)
  }
}
final case class AudioControlProxy(values: Vec[Float], name: Option[String])
  extends ControlProxyLike with AudioRated {
  private[synth] def factory: ControlFactoryLike = AudioControlProxy.factory
}
