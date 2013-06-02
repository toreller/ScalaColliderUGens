/*
 *  ControlProxyFactory.scala
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

import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import language.implicitConversions

object ControlProxyFactory {
  object Values {
    implicit def float    (x: Float) : Values = Values(Vector(x))
    implicit def double   (x: Double): Values = Values(Vector(x.toFloat))
    implicit def floatSeq (xs: IIdxSeq[Float]) : Values = Values(xs)
    implicit def doubleSeq(xs: IIdxSeq[Double]): Values = Values(xs.map(_.toFloat))
    private[ugen] val singleZero = Values(Vector(0f))
  }
  final case class Values(seq: IIdxSeq[Float])
}
final class ControlProxyFactory(name: String) {
  import ControlProxyFactory.Values

  def ir: ControlProxy = ir(Values.singleZero)
  def ir(values: Values): ControlProxy      = ControlProxy(scalar,  values.seq, Some(name))

  def kr: ControlProxy = kr(Values.singleZero)
  def kr(values: Values): ControlProxy      = ControlProxy(control, values.seq, Some(name))

  def tr: TrigControlProxy = tr(Values.singleZero)
  def tr(values: Values): TrigControlProxy  = TrigControlProxy     (values.seq, Some(name))

  def ar: AudioControlProxy = ar(Values.singleZero)
  def ar(values: Values): AudioControlProxy = AudioControlProxy    (values.seq, Some(name))
}

trait ControlFactoryLike {
  def build(b: UGenGraph.Builder, proxies: IIdxSeq[ControlProxyLike]): Map[ControlProxyLike, (UGen, Int)] = {
    var numChannels = 0
    val specialIndex = proxies.map(p => {
      numChannels += p.values.size
      b.addControl(p.values, p.name)
    }).head
    val ugen = makeUGen(numChannels, specialIndex)
    var offset = 0
    proxies.map(p => {
      val res = p -> (ugen, offset)
      offset += p.values.size
      res
    })(breakOut)
  }

  protected def makeUGen(numChannels: Int, specialIndex: Int): UGen
}

trait ControlProxyLike extends GE {
  // ---- constructor ----
  SynthGraph.builder.addControlProxy(this)

  def rate    : Rate
  private[synth] def factory: ControlFactoryLike
  def name    : Option[String]
  def values  : IIdxSeq[Float]

  /** Note: this expands to a single ControlUGenOutProxy for numChannels == 1,
    * otherwise to a sequence of proxies wrapped in UGenInGroup. Therefore,
    * {{{
    *    In.ar( "in".kr, 2 )
    * }}}
    * results in an `In` UGen, and doesn't rewrap into a UGenInGroup
    * (e.g. behaves like `In.ar( 0, 2 )` and not `In.ar( Seq( 0 ), 2 )` which
    * would mess up successive multi channel expansion.
    *
    * This is kind of a particular way of producing the proper `isWrapped` results.
    */
  final private[synth] def expand: UGenInLike = if (values.size == 1) {
    ControlUGenOutProxy(this, 0)
  } else {
    UGenInGroup(Vector.tabulate(values.size)(i => ControlUGenOutProxy(this, i)))
  }
}