/*
 *  ChannelProxy.scala
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

final case class ChannelProxy(elem: GE, index: Int) extends GE.Lazy {
  def rate = elem.rate

  override def toString = s"$elem.\\($index)"

  protected def makeUGens: UGenInLike = {
    // val _elem = elem.expand
    // _elem.unwrap(index)

    val res: UGenInLike = elem match {
      case in: UGenIn     => in
      case GESeq    (xs)  => xs(index % xs.length).expand
      case UGenInSeq(xs)  => xs(index % xs.length)
      case _ =>
        val _elem = elem.expand
        val out   = _elem.outputs
        val outF  = _elem.flatOutputs
        if (out.size == outF.size) {
          outF(index % outF.size)
        } else {
          UGenInGroup(Vector.tabulate(outF.size / out.size)(i => outF((index + i * out.size) % outF.size)))
        }
    }
    res
  }
}